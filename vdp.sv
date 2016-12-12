/* vdp.sv
 * Video Display Processor
 * Sega Master System
 * Celeste Neary, Jeremy Sonpar, & Suzz Glennon
 */
 
module vdp
  (output logic [3:0] vgaRed, vgaGreen, vgaBlue, // instead of comvid
   output logic       Hsync, Vsync,
   output logic       int_l,        // CPU interrupt output
   output logic       rw,           // VRAM write strobe (w = 0)
   output logic [7:0] dataOut,
   input  logic [7:0] dataIn, 
   input  logic       cpuClk, clk100, btnCpuReset,
   input  logic       csr_l,        // CPU-VDP read strobe
   input  logic       csw_l,        // CPU-VDP write strobe
   input  logic       mode,
   
   //debugging
   output logic [2:0] stateOutput,
   output bit [7:0] spriteRow, patCol, cramOut,
   output logic [8:0] spriteCol, shift,
   output logic delayLineSave,
   
   output logic [3:0] backdropAddrOut,
   output logic [7:0] backdropColor);
   

    bit rst, rst_l;
    assign rst = btnCpuReset; //BOTH ACTIVE LOW
    assign rst_l = btnCpuReset;
    bit dotClk;
    assign dotClk = cpuClk;
  
    enum logic [2:0] {Wait=3'b000, 
        Hold=3'b001, 
        vRAMSend=3'b010, 
        cRAMSend=3'b011, 
        vRAM=3'b100, 
        cRAM=3'b101, 
        cRAM_inc=3'b110, 
        vRAM_inc=3'b111} currS, nextS;
    assign stateOutput = currS;
    
    bit cas_l, ras_l; //column and row address strobe
  
    bit vertScroll;   // 1 = disable vert scrolling
    bit horizScroll;  // 1 = disable horiz scrolling
    bit overscanMask; // 1 = mask col 0
    bit ie1;          // line interrupt enable
    bit ec;           // shift sprites left by 8px
    bit m4;           // mode 4
    bit m2;           // mode 2 (see smspower for more details)
    bit no_sync;      // 1 = no sync + monochrome display
  
    bit blk;          // blank enable
    bit ie0;          // frame interrupt enable
    bit m1;           // mode 1
    bit m3;           // mode 3
    bit sprSize, size;         // sprite size select
    bit mag;          // sprite magnification
 
    //parse VDP registers
    assign vertScroll = optctrl0[7]; //inhibit
    assign horizScroll = optctrl0[6]; //inhibit
    assign overscanMask = optctrl0[5];
    assign ie1 = optctrl0[4];
    assign ec = optctrl0[3]; //sprite shift 8 pixels left
    assign m4 = optctrl0[2];
    assign m2 = optctrl0[1];
    assign no_sync = optctrl0[0];
  
    assign blk = optctrl1[6];
    assign ie0 = optctrl1[5];
    assign m1 = optctrl1[4];
    assign m3 = optctrl1[3];
    assign size = optctrl1[1];
    assign mag = optctrl1[0];
    
    assign sprSize = size;
 

    /* VRAM */
    bit vramWrite;
    bit [13:0] vramAddrCPU, vramAddrRead, vramAddrSpriteRead, cramAddrCPU, collectedAddr;
    bit [7:0] vramIn, vramOut, vramOutSprite, vramCpuOut;
    
    /* CPU vram */
    vram vram0(.clka(cpuClk), .wea(vramWrite), 
        .addra(vramAddrCPU), .dina(vramIn), .doutb(vramCpuOut), .clkb(cpuClk), .addrb(vramAddrCPU));
    
    /* pattern vram */
    vram vram1(.clka(cpuClk), .wea(vramWrite), 
        .addra(vramAddrCPU), .dina(vramIn), .doutb(vramOut), .clkb(dotClk), .addrb(vramAddrRead)); 
    
    /* sprite vram */  
    vram vram2(.clka(cpuClk), .wea(vramWrite), 
        .addra(vramAddrCPU), .dina(vramIn), .doutb(vramOutSprite), .clkb(dotClk), .addrb(vramAddrSpriteRead));

  

    bit [7:0] currPixel;
    bit [15:0] pixelAddr, vgaAddr;
    
    /* calculate the pixel address to give the PRAM */
    bit [255:0] pattern;
    bit patRst,patternReady,patternStored;
    
    bit pramWrite,bitEnOut;
    bit [15:0] pramSpriteAddr;
    
    logic [15:0] pixelAddr1reg, pixelAddr2reg, pixelAddr3reg;  
  
    bit hLineInt, vBlankInt;
    
    logic [9:0] vgaRow, vgaCol;
    assign vgaAddr = (16'd256 * vgaRow) + vgaCol;
  
  
    /* pattern collection majig */
    logic [13:0] nameBaseAddr, patternBaseAddr, satBaseAddr;
    logic [7:0] optctrl0, optctrl1, namebase, colorbase, patternbase, sprattr;
    logic [7:0] sprpat, backdrop, hscroll, vscroll, hli;
    
    //TODO get rid this is 4 debug
    assign backdropAddr2 = backdrop[3:0];
    
    /* vga output logic */ 
    vga_wrapper vga_wr(.clk(clk100), .btnCpuReset(btnCpuReset), .pramOut(currPixel),
                     .row2vdp(vgaRow), .col2vdp(vgaCol), .vgaRed(vgaRed),
                     .vgaGreen(vgaGreen), .vgaBlue(vgaBlue), .Vsync(Vsync),
                     .Hsync(Hsync), .hli(hli), .hLineInt(hLineInt), .vBlankInt(vBlankInt),
                     .vScrollVal(vscroll), .hScrollVal(hscroll), .vSInhib(vertScroll), .hSInhib(horizScroll),
                     .vgaRow(vgaOtherRow));

    
    /* logic for actual addresses*/
    assign nameBaseAddr = {namebase[3:1],11'b0};
    assign patternBaseAddr = {sprpat[2],13'b0};
    assign satBaseAddr = {sprattr[6:1], 8'b0};
  
    bit spriteRowReady, patRowReady, rowDone, bothReady;
    bit spriteRowReadyLatch, patRowReadyLatch;
    bit [8:0] spriteCol;
    assign bothReady = (spriteRowReadyLatch && patRowReadyLatch);
  
    always_ff @(posedge dotClk, negedge rst_l) begin
        if (~rst_l) begin
            patRowReadyLatch <= 1'b0;
            spriteRowReadyLatch <= 1'b0;
        end
        else
            if (patRowReady | spriteRowReady) begin
                patRowReadyLatch <= patRowReady ? 1'b1 : patRowReadyLatch;
                spriteRowReadyLatch <= spriteRowReady ? 1'b1 : spriteRowReadyLatch;
            end
            else begin 
                if (spriteRowReadyLatch&patRowReadyLatch) begin
                    spriteRowReadyLatch <= 1'b0;
                    patRowReadyLatch <= 1'b0;
                end
            end
        end

    /* outputs spriteRow */
    always_ff @(posedge dotClk, negedge rst) begin
        if (~rst) spriteRow <= 8'b0;
        else if (rowDone) begin
            if (spriteRow == 8'd191) spriteRow <= 8'b0;
            else spriteRow <= spriteRow + 1'b1;
        end
    end

    bit [255:0] spriteOut0, spriteOut1, spriteOut2, spriteOut3, spriteEn;
    bit [255:0] patOut0, patOut1, patOut2, patOut3, patEn, patPriority, patPalette;
  
    //PRAM AND CRAM STUFF ONLY
    bit [15:0] pixelAddr4reg;
    register #(16) addrConvert0(pramSpriteAddr, dotClk, 1'b0, 1'b1, pixelAddr1reg);
    register #(16) addrConvert1(pixelAddr1reg, dotClk, 1'b0, 1'b1, pixelAddr2reg); 
    register #(16) addrConvert2(pixelAddr2reg, dotClk, 1'b0, 1'b1, pixelAddr3reg);
    register #(16) addrConvert3(pixelAddr2reg, dotClk, 1'b0, 1'b1, pixelAddr4reg);
    
    bit pramWrite1,pramWrite2,enOut1, enOut2;
    register #(1) pramW0reg(pramWrite, dotClk, 1'b0, 1'b1, pramWrite1);
    register #(1) pramW1reg(pramWrite1, dotClk, 1'b0, 1'b1, pramWrite2); 

    register #(1) en0reg(bitEnOut, dotClk, 1'b0, 1'b1, enOut1);
    register #(1) en1reg(enOut1, dotClk, 1'b0, 1'b1, enOut2); 
  
    assign pramSpriteAddr = (spriteRow * 256) + spriteCol;
    assign pramWrite = (spriteCol<=9'd255) ? 1'b1 : 1'b0;  
    
    /* CRAM */
    bit cramWrite;
    bit [7:0] cramIn;//, cramOut;
    bit [4:0] cramAddr, cramAddrWrite;
    
    assign cramIn = dataIn;
    
    cram cram0(.clka(cpuClk), .wea(cramWrite), 
               .addra(cramAddrWrite), .dina(cramIn), .doutb(cramOut), .clkb(dotClk), .addrb(cramAddr));
    
    //debugging
    always_ff @(posedge cpuClk)
        if (cramAddr == {1'b1, backdrop[3:0]}) backdropColor <= cramOut;
  
    
    bit pramDBWrite;
    bit [7:0] pram0Out;
    bit [15:0] pramAddrDBRead; //, pramAddrRead;                
  
    /* write to PRAM and VGA read from PRAM */
    pram pram0(.clka(cpuClk), .wea(pramWrite2), .addra(pixelAddr3reg), .dina(cramOut),
             .clkb(clk100), .addrb(pramAddrDBRead), .doutb(pram0Out));
  
    //vBlankInt
    bit [9:0] vgaOtherRow;
    
    always_ff @(posedge clk100, negedge rst_l) begin
        if (~rst_l | vBlankInt) begin
            pramAddrDBRead <= 16'b0;
            pramDBWrite <= vBlankInt;
        end
        else if (pramAddrDBRead < 16'd49152) begin
            pramAddrDBRead <= pramAddrDBRead + 1;
            pramDBWrite <= 1'b1;
        end
        else pramDBWrite <= 1'b0;
    end
    
    pram pramDouble(.clka(clk100), .wea(pramDBWrite), .addra(pramAddrDBRead), .dina(pram0Out),
                          .clkb(clk100), .addrb(vgaAddr), .doutb(currPixel));
    
   
  
  
    /* sprite collector */                  
    bit sprCollideSet, nineSprSet;
    spriteCollector sC1(.spriteOut0(spriteOut0), .spriteOut1(spriteOut1), .spriteOut2(spriteOut2), .spriteOut3(spriteOut3),
                      .spriteEn(spriteEn), .clk(dotClk), .rst(~rst), .row(spriteRow), .ready(spriteRowReady),
                      .patternBaseAddr(patternBaseAddr), .satBaseAddr(satBaseAddr), .vramOut(vramOutSprite),
                      .vramAddr(vramAddrSpriteRead), .rowDone(rowDone),/*.satOffset(satOffset), .cramRow(cramRow), .currState(currState),
                      .spriteCount(spriteCount), .y(y), .rowDone(rowDone), .x1(x1), .mask1Part(mask1), .spr1(spr1), */
                      .size(sprSize), .sprCollide(sprCollideSet), .nineSpr(nineSprSet), .spriteShift(ec));

    /* pattern collector */
    logic [2:0] currState;
    bit readyOneBehind;
    logic [7:0] patCount;
    logic [31:0] patRowColors;
    logic [7:0] name0, name1, patternAddrOffset; //, patCol, patRow;
    
    patternCollector pc0(.patOut0(patOut0), .patOut1(patOut1), .patOut2(patOut2), .patOut3(patOut3),
                     .patEn(patEn), .patPriority(patPriority), .patPalette(patPalette), .clk(dotClk), .rst(~rst), .row(spriteRow),
                     .ready(patRowReady),.patternBaseAddr(patternBaseAddr),.nameBaseAddr(nameBaseAddr),
                     .vramOut(vramOut), .vramAddr(vramAddrRead), .rowDone(rowDone), .patCol(patCol),
                     .shift(shift), .delayLineSave(delayLineSave),
                     .hScroll(hscroll), .vScroll(vscroll), .hScrollInhib(horizScroll), .vScrollInhib(vertScroll)); 
      
      
      
    bit [255:0] patEnSave, patPriSave, pat0save, pat1save, pat2save, pat3save, patPalSave;
    always_ff @(posedge dotClk, negedge rst_l) begin
        if (~rst_l) begin
            pat0save <= 256'b0;
            pat1save <= 256'b0;
            pat2save <= 256'b0;
            pat3save <= 256'b0;
            patEnSave <= 256'b0;
            patPriSave <= 256'b0;
            patPalSave <= 256'b0;
        end
        else if (patRowReady) begin
            pat0save <= patOut0;
            pat1save <= patOut1;
            pat2save <= patOut2;
            pat3save <= patOut3;
            patEnSave <= patEn;
            patPriSave <= patPriority;
            patPalSave <= patPalette;
        end
    end

    outputSprites os0(.addr(cramAddr), .spriteOut0(spriteOut0), .spriteOut1(spriteOut1), .spriteOut2(spriteOut2), 
                      .spriteOut3(spriteOut3), .spriteEn(spriteEn), .patOut0(pat0save), .patOut1(pat1save), 
                      .patOut2(pat2save), .patOut3(pat3save), .patEn(patEnSave), .patPriority(patPriSave), .patPalette(patPalSave), 
                      .clk(dotClk), .we(bothReady), .rst(~rst),
                      .outputCol(spriteCol), .done(rowDone), .enOut(bitEnOut),
                      .sprBit0(sprBit0), .sprBit1(sprBit1), .sprBit2(sprBit2), .sprBit3(sprBit3),
                      .patBit0(patBit0), .patBit1(patBit1), .patBit2(patBit2), .patBit3(patBit3),
                      .backdropAddr(backdrop[3:0]),
                      .sprEnOut(sprEnOut), .patEnOut(patEnOut), .patPriOut(patPriOut), .rstLatch(rstLatch), .state(state));
    

  

    /* CPU Interface */
    bit [7:0] vdpStatusOut;
    assign dataOut = (csr_l == 1'b0) ? (mode ? vdpStatusOut : vramCpuOut) : 8'bz;
    
    /* CPU FSM */
    logic [2:0] transferParams;
    assign transferParams = {csw_l,csr_l,mode};
    
    bit vdpFSMenable, incrAddress;
    assign vdpFSMenable = (transferParams != 3'b110);

    /* holds data for 2 cycles */
    bit [7:0] D, heldData, heldData2;
    
    //CD FIX
    //assign D = cd;
    assign D = dataIn;
    assign vramIn = D;
    
    register #(8) cpu_holding(D, cpuClk, ~rst, vdpFSMenable, heldData); //first CPU byte
    register #(8) cpu_holding2(heldData, cpuClk, ~rst, vdpFSMenable, heldData2); //first CPU byte
    
    /* fsm output control */
    logic CRAMas,CRAMwrite;


    always_comb begin
        ras_l = 1'b1;
        cas_l = 1'b1;
        rw = 1'b1;
        CRAMwrite = 1'b0;
        incrAddress = 1'b0;
        if (vdpFSMenable) begin
            case (currS)
                Wait: begin
                    nextS = (transferParams == 3'b101) ? Wait : Hold;
                end
                Hold: begin
                    nextS = (dataIn[7:6]==2'b10) ? Wait : ((dataIn[7:6]==2'b11) ? cRAMSend : vRAMSend);
                    ras_l = ~(nextS == cRAMSend || nextS == vRAMSend);
                end
                vRAMSend: begin
                    nextS = vRAM;
                    rw = (transferParams != 3'b010);
                    cas_l = 1'b0;
                end
                vRAM: begin
                    nextS = (transferParams == 3'b011) ? Hold : ((transferParams == 3'b010) ? vRAM_inc : Wait);
                    rw = (transferParams != 3'b010);
                    incrAddress = (transferParams == 3'b010) ? 1'b1 : 1'b0;
                end
                vRAM_inc: begin
                    nextS = (transferParams == 3'b011) ? Hold : ((transferParams == 3'b010) ? vRAM_inc : Wait);
                    rw = (transferParams != 3'b010);
                    incrAddress = 1'b1;
                end
                cRAMSend: begin
                    nextS = cRAM;
                    cas_l = 1'b0;
                    CRAMwrite = 1'b1;
                end
                cRAM: begin
                    nextS = (transferParams == 3'b011) ? Hold : ((transferParams == 3'b010) ? cRAM_inc : Wait);
                    CRAMwrite = (transferParams != 3'b011);
                    incrAddress = (transferParams == 3'b010) ? 1'b1 : 1'b0;
                end
                cRAM_inc: begin
                    nextS = (transferParams == 3'b011) ? Hold : ((transferParams == 3'b010) ? cRAM_inc : Wait);
                    CRAMwrite = (transferParams != 3'b011);
                    incrAddress = 1'b1;
                end
            endcase
        end
        else begin
            nextS = currS;
        end
    end
  
    always_ff @(posedge cpuClk)
        vramWrite <= vdpFSMenable&&(~rw);
    
    always_ff @(posedge cpuClk)
        cramWrite <= vdpFSMenable&&(CRAMwrite);

    /* CPU Write to VDP */
    bit [10:0] en;
    bit masterEn;
    
    assign masterEn = vdpFSMenable && (dataIn[7:6]==2'b10) && (currS==Hold);
    registerSelectDecoder rSD(en, masterEn, dataIn);
    
    addrCollector addrC(.clock(cpuClk), 
                      .reset(~rst_l),
                      .en8(~ras_l), 
                      .en6(~cas_l), 
                      .increment(incrAddress), 
                      .D(heldData), 
                      .Q(collectedAddr));
                      
    assign vramAddrCPU = collectedAddr;
    assign cramAddrWrite = collectedAddr[4:0];

    /* write-only registers */
    registerRstVal #(8) reg0(heldData, /*8'h36*/8'b0, cpuClk, ~rst_l, en[0], optctrl0);
    registerRstVal #(8) reg1(heldData, /*8'ha0*/8'b0, cpuClk, ~rst_l, en[1], optctrl1);
    registerRstVal #(8) reg2(heldData, /*8'hff*/8'b0, cpuClk, ~rst_l, en[2], namebase);
    registerRstVal #(8) reg3(heldData, /*8'hff*/8'b0, cpuClk, ~rst_l, en[3], colorbase); // no effect
    registerRstVal #(8) reg4(heldData, /*8'hff*/8'b0, cpuClk, ~rst_l, en[4], patternbase); // no effect
    registerRstVal #(8) reg5(heldData, /*8'hff*/8'b0, cpuClk, ~rst_l, en[5], sprattr);
    registerRstVal #(8) reg6(heldData, /*8'hfb*/8'b0, cpuClk, ~rst_l, en[6], sprpat);
    registerRstVal #(8) reg7(heldData, 8'h00, cpuClk, ~rst_l, en[7], backdrop);
    registerRstVal #(8) reg8(heldData, 8'h00, cpuClk, ~rst_l, en[8], hscroll); //hscroll
    registerRstVal #(8) reg9(heldData, 8'h00, cpuClk, ~rst_l, en[9], vscroll); //vscroll
    registerRstVal #(8) reg10(heldData, /*8'hff*/8'b0, cpuClk, ~rst_l, en[10], hli); //horiz line interupt
    
    bit [7:0] vdpStatusIn;
    bit statEn, vbl, nineSpr, sprCollide; //TODO assign these
    assign statEn = 1'b1;
    assign vdpStatusIn = {vbl, nineSpr, sprCollide, 5'b0};
    register #(8) status(vdpStatusIn, cpuClk, ~rst_l, statEn, vdpStatusOut);
    
    
    /* state transition */
    always_ff@(posedge cpuClk, negedge btnCpuReset) begin
        if (~btnCpuReset) begin
            currS <= Wait;
        end
        else begin
            currS <= nextS;
        end
    end

    //interrupts
    //hLineInt, vBlankInt
    bit hLineIntLatch, vBlankIntLatch, statusRead; //past tense read
    bit sprCollideLatch, nineSprLatch;
  
    always_ff @(posedge cpuClk, posedge hLineInt, negedge rst_l)
        if (~rst_l) hLineIntLatch <= 1'b0;
        else if (hLineInt) hLineIntLatch <= 1'b1;
        else if (statusRead) hLineIntLatch <= 1'b0;
    
    always_ff @(posedge cpuClk, posedge vBlankInt, negedge rst_l)
        if (~rst_l) vBlankIntLatch <= 1'b0;
        else if (vBlankInt) vBlankIntLatch <= 1'b1;
        else if (statusRead) vBlankIntLatch <= 1'b0;
  
    always_ff @(posedge cpuClk, posedge nineSprSet, negedge rst_l)
        if (~rst_l) nineSprLatch <= 1'b0;
        else if (nineSprSet) nineSprLatch <= 1'b1;
        else if (statusRead) nineSprLatch <= 1'b0;
        
    always_ff @(posedge cpuClk, posedge sprCollideSet, negedge rst_l)
        if (~rst_l) sprCollideLatch <= 1'b0;
        else if (sprCollideSet) sprCollideLatch <= 1'b1;
        else if (statusRead) sprCollideLatch <= 1'b0;
                
  //output int_l, save to regs
    always_comb begin
        statusRead = (~csr_l&&mode) ? 1'b1 : 1'b0;
        vbl = vBlankInt|vBlankIntLatch;
        int_l = ~(ie1||ie0) ? 1'b1 : (ie0 ? ~(vBlankInt|vBlankIntLatch|hLineInt|hLineIntLatch) : ~(hLineInt|hLineIntLatch)); 
    
        nineSpr = nineSprSet|nineSprLatch;
        sprCollide = sprCollideSet|sprCollideLatch;
    end
  
endmodule: vdp

/* this module takes the destination from the CPU
   and enables the right register */
module registerSelectDecoder
  (output logic [10:0] en,
   input logic masterEn,
   inout  logic [7:0] dest);

  always_comb begin
    if (masterEn)
      case (dest[3:0]) //lowest 4 bits, see 2.1.2
        4'b0000 : en = 11'b00000000001;
        4'b0001 : en = 11'b00000000010;
        4'b0010 : en = 11'b00000000100;
        4'b0011 : en = 11'b00000001000;
        4'b0100 : en = 11'b00000010000;
        4'b0101 : en = 11'b00000100000;
        4'b0110 : en = 11'b00001000000;
        4'b0111 : en = 11'b00010000000;
        4'b1000 : en = 11'b00100000000;
        4'b1001 : en = 11'b01000000000;
        4'b1010 : en = 11'b10000000000;
        default : en = 11'b00000000000;
      endcase
    else begin
     // dest is 8'bz;
      en = 11'b0;
    end
  end

endmodule: registerSelectDecoder