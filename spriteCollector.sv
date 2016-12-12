module spriteCollector(
    output logic [255:0] spriteOut0, spriteOut1, spriteOut2, spriteOut3,
    output logic [255:0] spriteEn,  //0 = no sprite
    input  logic         clk, rst,
    input  logic [7:0]   row,
    output logic         ready,
    input  logic [13:0]  patternBaseAddr, satBaseAddr,
    input  logic [7:0]   vramOut,
    output logic [13:0]  vramAddr,
    input logic rowDone, size,
    output logic sprCollide, nineSpr,
    input logic spriteShift);
    
    
    enum logic [2:0] {Wait, Start, GetY, SaveY, SaveX, SaveN, StorePattern} currS, nextS;
        
    
    assign currState = currS;
        
    logic [7:0] satOffset;
    logic [7:0] x, n, y;
    logic [15:0] patternAddrOffset;
    logic [7:0] x0, x2, x3, x4, x5, x6, x7,x1;
    logic [31:0] spr0, spr2, spr3, spr4, spr5, spr6, spr7, spr1;
    logic [3:0] spriteCount;
    logic [1:0] cramRow;
    

    bit [4:0] vSize;
    assign vSize = (size) ? 5'd15 : 5'd7;

    always_comb begin
        unique case (currS)
            Wait: nextS = (rowDone) ? GetY : Wait;
            Start: nextS = GetY;
            GetY: nextS = SaveY;
            SaveY: nextS = ((vramOut == 8'hd0)||(satOffset==7'd64)) ? Wait : (((row - (vramOut - 1'b1)) <= vSize) ? SaveX : GetY);
            SaveX: nextS = (spriteCount==4'd8) ? Wait : SaveN;
            SaveN: nextS = StorePattern;
            StorePattern: nextS = (cramRow == 2'd3) ? ((satOffset==7'd64) ? Wait : GetY) : StorePattern;
        endcase
    end
    
    //handle vramAddr, ready
    always_comb begin
        ready = (nextS == Wait)&&(currS != Wait);
        unique case (currS)
            Wait: vramAddr = satBaseAddr;
            Start: vramAddr = satBaseAddr;
            GetY: vramAddr = satBaseAddr + satOffset;
            SaveY: 
                if (nextS == GetY)
                    vramAddr = satBaseAddr + satOffset;
                else
                    vramAddr = satBaseAddr + 8'h80 + ((satOffset-1) << 1);
            SaveX: vramAddr = satBaseAddr + 8'h81 + ((satOffset-1) << 1);
            SaveN: vramAddr = 32*vramOut + patternBaseAddr + ((row - y) *4);
            StorePattern: vramAddr = patternBaseAddr + patternAddrOffset + cramRow + 1'b1;
        endcase
    end


    //transition and some logic stuffs
    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            cramRow <= 2'b0;
            spriteCount <= 3'b0;
            satOffset <= 1'b0;
            spr0 <= 32'b0;
            spr1 <= 32'b0;
            spr2 <= 32'b0;
            spr3 <= 32'b0;
            spr4 <= 32'b0;
            spr5 <= 32'b0;
            spr6 <= 32'b0;
            spr7 <= 32'b0;
        end
        else begin
            unique case (currS) 
                Wait: begin //start of new row
                    if (rowDone) begin
                        cramRow <= 2'b0;
                        spriteCount <= 3'b0;
                        satOffset <= 1'b0;
                        spr0 <= 32'b0;
                        spr1 <= 32'b0;
                        spr2 <= 32'b0;
                        spr3 <= 32'b0;
                        spr4 <= 32'b0;
                        spr5 <= 32'b0;
                        spr6 <= 32'b0;
                        spr7 <= 32'b0;
                    end                
                  
                end
                Start: begin //start of new row
                    cramRow <= 2'b0;
                    spriteCount <= 3'b0;
                    satOffset <= 1'b0;
                    spr0 <= 32'b0;
                    spr1 <= 32'b0;
                    spr2 <= 32'b0;
                    spr3 <= 32'b0;
                    spr4 <= 32'b0;
                    spr5 <= 32'b0;
                    spr6 <= 32'b0;
                    spr7 <= 32'b0;
                end
                GetY: begin
                    satOffset <= satOffset+1;
                end
                SaveY: begin
                    cramRow <= 2'b0;
                    y <= vramOut - 1'b1;
                end
                SaveX: begin
                    unique case (spriteCount)
                        4'd0: x0 <= vramOut;
                        4'd1: x1 <= vramOut;
                        4'd2: x2 <= vramOut;
                        4'd3: x3 <= vramOut;
                        4'd4: x4 <= vramOut;
                        4'd5: x5 <= vramOut;
                        4'd6: x6 <= vramOut;
                        4'd7: x7 <= vramOut;
                    endcase
                end
                SaveN: begin
                    patternAddrOffset <= 32*vramOut + ((row - y) *4);
                    
                end
                StorePattern: begin
                    if (cramRow == 2'd3) begin
                        spriteCount <= spriteCount + 1'b1;
                    end
                    if (cramRow == 2'd0) begin
                        unique case (spriteCount)
                            4'd0: spr0[7:0] <= vramOut;
                            4'd1: spr1[7:0] <= vramOut;
                            4'd2: spr2[7:0] <= vramOut;
                            4'd3: spr3[7:0] <= vramOut;
                            4'd4: spr4[7:0] <= vramOut;
                            4'd5: spr5[7:0] <= vramOut;
                            4'd6: spr6[7:0] <= vramOut;
                            4'd7: spr7[7:0] <= vramOut;
                        endcase
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd1) begin
                        unique case (spriteCount)
                            4'd0: spr0[15:8] <= vramOut;
                            4'd1: spr1[15:8] <= vramOut;
                            4'd2: spr2[15:8] <= vramOut;
                            4'd3: spr3[15:8] <= vramOut;
                            4'd4: spr4[15:8] <= vramOut;
                            4'd5: spr5[15:8] <= vramOut;
                            4'd6: spr6[15:8] <= vramOut;
                            4'd7: spr7[15:8] <= vramOut;
                        endcase
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd2) begin
                        unique case (spriteCount)
                            4'd0: spr0[23:16] <= vramOut;
                            4'd1: spr1[23:16] <= vramOut;
                            4'd2: spr2[23:16] <= vramOut;
                            4'd3: spr3[23:16] <= vramOut;
                            4'd4: spr4[23:16] <= vramOut;
                            4'd5: spr5[23:16] <= vramOut;
                            4'd6: spr6[23:16] <= vramOut;
                            4'd7: spr7[23:16] <= vramOut;
                        endcase
                        cramRow <= cramRow + 1'b1;
                    end
                    if (cramRow == 2'd3) begin
                        unique case (spriteCount)
                            4'd0: spr0[31:24] <= vramOut;
                            4'd1: spr1[31:24] <= vramOut;
                            4'd2: spr2[31:24] <= vramOut;
                            4'd3: spr3[31:24] <= vramOut;
                            4'd4: spr4[31:24] <= vramOut;
                            4'd5: spr5[31:24] <= vramOut;
                            4'd6: spr6[31:24] <= vramOut;
                            4'd7: spr7[31:24] <= vramOut;
                        endcase
                    end      
                end
            endcase
        end
    end
    
    
    always_ff @(posedge clk, posedge rst) 
        if (rst) currS <= Start;
        else currS <= nextS;
        
        
    //masking and getting sprite outputs
    bit [255:0] mask0,mask1,mask2,mask3,mask4,mask5,mask6,mask7;
    bit [255:0] l00,l01,l02,l03,l10,l11,l12,l13,l20,l21,l22,l23;
    bit [255:0] l30,l31,l32,l33,l40,l41,l42,l43,l50,l51,l52,l53;
    bit [255:0] l60,l61,l62,l63,l70,l71,l72;
    
    createMask m0(spr0, x0, mask0, l00,l01,l02,l03),
           m1(spr1, x1, mask1, l10,l11,l12,l13),
           m2(spr2, x2, mask2, l20,l21,l22,l23),
           m3(spr3, x3, mask3, l30,l31,l32,l33),
           m4(spr4, x4, mask4, l40,l41,l42,l43),
           m5(spr5, x5, mask5, l50,l51,l52,l53),
           m6(spr6, x6, mask6, l60,l61,l62,l63),
           m7(spr7, x7, mask7, l70,l71,l72,l73);
    
    bit [255:0] t00,t01,t02,t03,t10,t11,t12,t13,t20,t21,t22,t23;
    bit [255:0] t30,t31,t32,t33,t40,t41,t42,t43,t50,t51,t52,t53, final0, final1, final2, final3;
    masker tempm0(l70,l71,l72,l73, l60,l61,l62,l63, mask6,t00,t01,t02,t03),
           tempm1(t00,t01,t02,t03, l50,l51,l52,l53, mask5,t10,t11,t12,t13),
           tempm2(t10,t11,t12,t13, l40,l41,l42,l43, mask4,t20,t21,t22,t23),
           tempm3(t20,t21,t22,t23, l30,l31,l32,l33, mask3,t30,t31,t32,t33),
           tempm4(t30,t31,t32,t33, l20,l21,l22,l23, mask2,t40,t41,t42,t43),
           tempm5(t40,t41,t42,t43, l10,l11,l12,l13, mask1,t50,t51,t52,t53),
           FINAL(t50,t51,t52,t53, l00,l01,l02,l03, mask0,final0,final1,final2,final3);
           
   
    
   //FLAGS!!!!!!
   bit ol1,ol2,ol3,ol4,ol5,ol6,ol7;
   bit [255:0] enOut1, enOut2,enOut3,enOut4,enOut5,enOut6;
   
   weirdFlagChecker wfc1(~mask0,~mask1,enOut1,ol1),
                    wfc2(enOut1,~mask2,enOut2,ol2),
                    wfc3(enOut2,~mask3,enOut3,ol3),   
                    wfc4(enOut3,~mask4,enOut4,ol4),
                    wfc5(enOut4,~mask5,enOut5,ol5),
                    wfc6(enOut5,~mask6,enOut6,ol6),
                    wfc7(enOut6,~mask7,enOut7,ol7);
   always_comb begin
       if (spriteShift) begin
            spriteOut0 = final0<<8;
            spriteOut1 = final1<<8;
            spriteOut2 = final2<<8;
            spriteOut3 = final3<<8;
            spriteEn = (~(mask0&mask1&mask2&mask3&mask4&mask5&mask6&mask7))<<8;
       end
       else begin
            spriteOut0 = final0;
            spriteOut1 = final1;
            spriteOut2 = final2;
            spriteOut3 = final3;
            spriteEn = ~(mask0&mask1&mask2&mask3&mask4&mask5&mask6&mask7);
       end
   end
                                                            
   always_comb begin
       nineSpr = (currS==SaveX)&&(spriteCount==4'd8);
       sprCollide = ol1|ol2|ol3|ol4|ol5|ol6|ol7; 
   end
   //end flags

          
    
    assign mask1Part = mask1[255:240];
                    
endmodule: spriteCollector

module weirdFlagChecker(
    input logic [255:0] enables1, enables2,
    output logic [255:0] enablesOut,
    output logic overlap);

    assign enablesOut = enables1|enables2;
    assign overlap = ((enables1&enables2)!=256'b0);

endmodule: weirdFlagChecker


module masker(
    input  logic [255:0] lowerP0,lowerP1,lowerP2,lowerP3,
    input logic [255:0] higherP0, higherP1, higherP2, higherP3, mask,
    output logic [255:0] out0,out1,out2,out3);

    always_comb begin
        out0 = (lowerP0 & mask) | higherP0;
        out1 = (lowerP1 & mask) | higherP1;
        out2 = (lowerP2 & mask) | higherP2;
        out3 = (lowerP3 & mask) | higherP3;
    end
    
endmodule: masker

module createMask(
    input  logic [31:0]  sprite,
    input  logic [7:0]   x,
    output logic [255:0] mask,
    output logic [255:0] line0, line1, line2, line3);
    
    logic [8:0] shift;
    assign shift = 9'd256 - x;
    
    logic [263:0] temp0,temp1,temp2,temp3, maskTemp, transparentTemp;
    
    always_comb begin
        temp0 = {256'b0, sprite[7:0]} << shift;
        temp1 = {256'b0, sprite[15:8]} << shift;
        temp2 = {256'b0, sprite[23:16]} << shift;
        temp3 = {256'b0, sprite[31:24]} << shift;
        
        line0 = temp0[263:8];
        line1 = temp1[263:8];
        line2 = temp2[263:8];
        line3 = temp3[263:8];
        
        transparentTemp = {256'b0, (sprite[7:0]|sprite[15:8]|sprite[23:16]|sprite[31:24])};
        
        maskTemp = ~(transparentTemp << shift);
        mask = maskTemp[263:8];
    end   
    
endmodule: createMask    
