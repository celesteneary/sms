module patternCollector(
    output logic [255:0] patOut0, patOut1, patOut2, patOut3,
    output logic [255:0] patEn, patPriority, patPalette,  //0 = no sprite
    input  logic         clk, rst,
    input  logic [7:0]   row,
    output logic         ready,
    input  logic [13:0]  patternBaseAddr, nameBaseAddr,
    input  logic [7:0]   vramOut,
    output logic [13:0]  vramAddr,
    input logic rowDone,
    input logic [7:0] hScroll, vScroll,
    input logic hScrollInhib, vScrollInhib,
    //debugging
    output logic [2:0] currState,
    output bit readyOneBehind,
    output logic [7:0] patCount,
    output logic [31:0] patRowColors,
    output logic [7:0] name0, name1, patCol,
    output logic [13:0] patternAddrOffset,
    output logic delayLineSave, patRst,
    output logic [8:0] shift);
    
    
    enum logic [2:0] {PreStart, Start, Wait, SaveName0, SaveName1, StorePattern} currS, nextS;
        
    
    assign currState = currS;
        
    logic [7:0] x, n; /*patCount, patCol, patRow; */ 
    //logic [31:0] patRowColors; 
    logic [1:0] cramRow;
   
    //bit patRst, lineSave;
    //bit [7:0] name0, name1;
    //bit [13:0] patternAddrOffset;
   
   
    always_comb begin
        unique case (currS)
            PreStart: nextS = Start;
            Wait: nextS = (rowDone) ? Start : Wait;
            Start: nextS = SaveName0;
            SaveName0: nextS = SaveName1; 
            SaveName1: nextS = StorePattern;
            StorePattern: nextS = (cramRow == 2'd3) ? ((patCount==8'd32) ? Wait : Start) : StorePattern;
        endcase
    end

    //bit readyOneBehind;
    bit readyTwoBehind;
    always_ff @(posedge clk) begin
        ready <= readyOneBehind;
        readyOneBehind <= readyTwoBehind;
    end

    logic [8:0] scrolledRow;
    assign scrolledRow = (vScroll+row>=224) ?  (vScroll+row-224) : (vScroll+row);

    always_comb begin
        readyTwoBehind = (nextS == Wait)&&(currS != Wait);
        unique case (currS)
            PreStart: vramAddr = nameBaseAddr;
            Wait: vramAddr = nameBaseAddr;
            Start: vramAddr = nameBaseAddr + 64*(scrolledRow[7:3]) + 2*patCol + 1; //32 patterns across * 2 bytes/pattern = 64
            SaveName0: vramAddr = nameBaseAddr + 64*(scrolledRow[7:3]) + 2*patCol;
            SaveName1: 
                unique case (name0[2]) //vert flip
                    1'b0: vramAddr = patternBaseAddr + 32*({name0[0], vramOut}) + ((scrolledRow[2:0]) *4);
                    1'b1: vramAddr = patternBaseAddr + 32*({name0[0], vramOut}) + (((7-(scrolledRow[2:0])) *4));
                endcase
            StorePattern: vramAddr = patternBaseAddr + patternAddrOffset + cramRow + 1'b1;
        endcase
    end


    //transition and some logic stuffs
    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            cramRow <= 2'b0;
            patCol <= 8'b0;
            patCount <= 8'b0;
            patternAddrOffset <= 0;
            name0 <= 8'b0;
            name1 <= 8'b1;
            patRowColors <= 32'b0;
        end
        else begin
            
            unique case (currS)
                Wait: begin
                    cramRow <= 2'b0;
                    patCol <= 8'b0;
                    patCount <= 8'b0;
                    patternAddrOffset <= 14'b0;
                    patRowColors <= 32'b0;
                end
                PreStart: begin
                    cramRow <= 2'b0;
                    patCol <= 8'b0;
                    patCount <= 8'b0;
                    patternAddrOffset <= 14'b0;
                end
                Start: begin
                    cramRow <= 2'b0;
                    patternAddrOffset <= 14'b0;
                    patRowColors <= 32'b0;
                end
                SaveName0: begin
                    name0 <= vramOut;
                end
                SaveName1: begin
                    name1 <= vramOut;
                    patternAddrOffset <= (name0[2]) ? (32*({name0[0], vramOut}) + ((7-(scrolledRow[2:0])) *4)) : (
                                                       32*({name0[0], vramOut}) + ((scrolledRow[2:0]) *4));
                    patCol <= (patCol==8'd31) ? 8'd0 : patCol + 1'b1;
                    patCount <= patCount + 1'b1;
                end
                StorePattern: begin
                    if (cramRow == 2'd0) begin
                        patRowColors[7:0] <= vramOut;
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd1) begin
                        patRowColors[15:8] <= vramOut;
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd2) begin
                        patRowColors[23:16] <= vramOut;
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd3) begin
                        patRowColors[31:24] <= vramOut;
                    end      
                end
            endcase
        end
    end
    
    
    always_ff @(posedge clk, posedge rst) 
        if (rst) currS <= PreStart;
        else currS <= nextS;
        
        
    //masking and getting sprite outputs
    bit [255:0] patMask;
    
    assign patRst = rst || ((rowDone)&&(currS==Wait));
    
    assign lineSave = (currS==StorePattern)&&(nextS!=StorePattern);
    bit delayLineSave, delayPatRst;
    always_ff @(posedge clk) begin
        delayLineSave <= lineSave;
        delayPatRst <= patRst;
    end
    
    createPatOut patMakeStuff(patRowColors, patCol, name0, patOut0,patOut1,patOut2,patOut3, 
                              patEn, patPriority, patPalette, delayPatRst, clk, delayLineSave, shift, 
                              hScroll, hScrollInhib, row);
                    
endmodule: patternCollector


module createPatOut(
    input  logic [31:0]  pattern,
    input  logic [7:0]   patCol, name,
    output logic [255:0] line0, line1, line2, line3, lineEn, linePriority, linePalette,
    input logic rst, clk, lineSave,
    output logic [8:0] shift,
    input logic [7:0] hScroll,
    input logic inhib,
    input logic [7:0] row); 
        
    //logic [8:0] shift;
    //patCol is 1 greater than it should be
    assign shift = (patCol==8'b0) ? 9'd8 : 9'd256 - (8*(patCol-1));
    
    bit patPriority;
    assign patPriority = name[4];
    
    logic [263:0] temp0,temp1,temp2,temp3;
    logic [255:0] line0temp,line1temp,line2temp,line3temp, priorityTemp, mask, paletteTemp;
    logic [263:0] transparentTemp,priorityTemp1, maskTemp, paletteTempLong;
    bit [511:0] line0Big, line1Big, line2Big, line3Big, lineEnBig, linePriBig, linePalBig;
    
    always_ff @(posedge clk, posedge rst)
        if (rst) begin
            line0 <= 256'd0;
            line1 <= 256'd0;
            line2 <= 256'd0;
            line3 <= 256'd0;
            lineEn <= 256'd0;
            linePriority <= 256'd0;
            linePalette <= 256'd0;
        end
        else if (lineSave) begin
            line0 <= line0 | line0Big[255:0] | line0Big[511:256];
            line1 <= line1 | line1Big[255:0] | line1Big[511:256];
            line2 <= line2 | line2Big[255:0] | line2Big[511:256];
            line3 <= line3 | line3Big[255:0] | line3Big[511:256];
            lineEn <= lineEn | lineEnBig[255:0] | lineEnBig[511:256];
            linePriority <= linePriority | linePriBig[255:0] | linePriBig[511:256];
            linePalette <= linePalette | linePalBig[255:0] | linePalBig[511:256];
        end
    
    
    bit [7:0] flippyBits0,flippyBits1,flippyBits2,flippyBits3;
    bit flip;
    assign flip = name[1];
    
    
    
    always_comb begin
        line0Big = (inhib&&(row<8'd16)) ? line0temp : ({line0temp,256'b0})>>hScroll;
        line1Big = (inhib&&(row<8'd16)) ? line1temp : ({line1temp,256'b0})>>hScroll;
        line2Big = (inhib&&(row<8'd16)) ? line2temp : ({line2temp,256'b0})>>hScroll;
        line3Big = (inhib&&(row<8'd16)) ? line3temp : ({line3temp,256'b0})>>hScroll;
        lineEnBig = (inhib&&(row<8'd16)) ? mask : ({mask,256'b0})>>hScroll;
        linePriBig = (inhib&&(row<8'd16)) ? priorityTemp : ({priorityTemp,256'b0})>>hScroll;
        linePalBig = (inhib&&(row<8'd16)) ? paletteTemp : ({paletteTemp,256'b0})>>hScroll;
        
        flippyBits0 = flip ? {pattern[0],pattern[1],pattern[2],pattern[3],pattern[4],pattern[5],pattern[6],pattern[7]} : pattern[7:0];
        flippyBits1 = flip ? {pattern[8],pattern[9],pattern[10],pattern[11],pattern[12],pattern[13],pattern[14],pattern[15]} : pattern[15:8];
        flippyBits2 = flip ? {pattern[16],pattern[17],pattern[18],pattern[19],pattern[20],pattern[21],pattern[22],pattern[23]} : pattern[23:16];
        flippyBits3 = flip ? {pattern[24],pattern[25],pattern[26],pattern[27],pattern[28],pattern[29],pattern[30],pattern[31]} : pattern[31:24];
        
        temp0 = {256'b0, flippyBits0} << shift;
        temp1 = {256'b0, flippyBits1} << shift;
        temp2 = {256'b0, flippyBits2} << shift;
        temp3 = {256'b0, flippyBits3} << shift;
        
        line0temp = temp0[263:8];
        line1temp = temp1[263:8];
        line2temp = temp2[263:8];
        line3temp = temp3[263:8];
        
        transparentTemp = {256'b0, (flippyBits0|flippyBits1|flippyBits2|flippyBits3)};
        
        
        maskTemp = (transparentTemp << shift);
        mask = maskTemp[263:8];
        
        
        
        priorityTemp1 = (patPriority) ? (263'hFF)<<shift : 263'b0;
        priorityTemp = priorityTemp1[263:8];
        
        paletteTempLong = (name[3]) ? (263'hFF)<<shift : 263'b0;
        paletteTemp = paletteTempLong[263:8];
    end   
    
    
    
    
endmodule: createPatOut 
