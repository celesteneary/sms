/* lib.sv
 * 18-545 Team Swag Master System
 * Celeste Neary
 * Suzz Glennon
 * Jeremy Sonpar
 */

/* register taken from Celeste's 240 lib - replace if you have a better one */
module register
  #(parameter WIDTH = 6)
  (input  logic [WIDTH-1:0] D,
   input  logic clock, reset, en,
   output logic [WIDTH-1:0] Q);

  always_ff @(posedge clock, posedge reset) begin
    if (reset)
      Q <= 0;
    else if (en) Q <= D;
  end

endmodule: register

module registerRstVal
  #(parameter WIDTH = 6)
  (input  logic [WIDTH-1:0] D, rstVal,
   input  logic clock, reset, en,
   output logic [WIDTH-1:0] Q);

  always_ff @(posedge clock, posedge reset) begin
    if (reset)
      Q <= rstVal;
    else if (en) Q <= D;
  end

endmodule: registerRstVal


module addrCollector
  (input logic clock, reset, en8, en6, increment,
   input logic [7:0] D,
   output logic [13:0] Q);

  logic inc_en_l;

  always_ff @(posedge clock, posedge reset) begin
    if (reset) Q <= 14'b0;
    else if (en8)
      Q[7:0] <= D;
    else if (en6)
      Q[13:8] <= D[5:0];
    else if (increment)
      Q[13:0] <= Q[13:0] + 1'b1;
  end


endmodule: addrCollector

module counter
  #(parameter WIDTH = 6)
  (input  logic clock, reset, en,
   output logic [WIDTH-1:0] sum);

  always_ff @(posedge clock, posedge reset) begin
    if (reset) sum <= 1'b0;
    else if (en) sum <= sum + 1'b1;
  end
  
endmodule: counter

module divide_clock_by_two(
  input logic in_clk, rst, 
  output logic out_clk);

  always_ff @(posedge in_clk, posedge rst) begin
    if (rst) out_clk <= 1'b0;
    else out_clk <= ~out_clk;
  end

endmodule: divide_clock_by_two


module outputSprites
    (output logic [4:0]   addr,
     output logic [8:0]   outputCol,
     input  logic [255:0] spriteOut0, spriteOut1, spriteOut2, spriteOut3, spriteEn,
     input  logic [255:0] patOut0, patOut1, patOut2, patOut3, patEn, patPriority, patPalette,
     input  logic         clk, we, rst,
     output logic done, enOut, patPalOut,
     input logic [3:0] backdropAddr,
     
     //debugging outputs
     output logic sprBit0,sprBit1,sprBit2,sprBit3,patBit0,patBit1,patBit2,patBit3,
     output logic sprEnOut, patEnOut, patPriOut,
     output logic rstLatch,
     output logic [2:0] state);
     
    assign state = currS;

    assign done = (outputCol>= 9'd255);

    enum logic [2:0] {Wait, Iterate} currS, nextS;

    
    logic rstLatch;
    
    always_ff @(posedge clk, posedge rst)
        if (rst)
            rstLatch <= rst;
        else rstLatch <= 1'b0;    
        

    always_comb
        unique case (currS)
            Wait: nextS = (we) ? Iterate : Wait;
            Iterate: nextS = (outputCol>= 9'd255) ? Wait : Iterate;
        endcase
        
    always_ff @(posedge clk, posedge rst) begin
        if (rst) outputCol <= 0;
        else
            unique case (nextS) 
                Iterate: outputCol <= outputCol + 1'b1;
                Wait: outputCol <= 0;
            endcase
    end



    //bit sprBit0,sprBit1,sprBit2,sprBit3,patBit0,patBit1,patBit2,patBit3;

    pisoShiftRegister #(256) sr0(spriteOut0,clk,we,rst,sprBit0);
    pisoShiftRegister #(256) sr1(spriteOut1,clk,we,rst,sprBit1);
    pisoShiftRegister #(256) sr2(spriteOut2,clk,we,rst,sprBit2);
    pisoShiftRegister #(256) sr3(spriteOut3,clk,we,rst,sprBit3);



    pisoShiftRegister #(256) pat0(patOut0,clk,we,rst,patBit0);
    pisoShiftRegister #(256) pat1(patOut1,clk,we,rst,patBit1);
    pisoShiftRegister #(256) pat2(patOut2,clk,we,rst,patBit2);
    pisoShiftRegister #(256) pat3(patOut3,clk,we,rst,patBit3);
    
    
    //logic sprEnOut, patEnOut, patPriOut;
    pisoShiftRegister #(256) en1(spriteEn,clk,we,rst,sprEnOut);
        
    pisoShiftRegister #(256) en2(patEn,clk,we,rst,patEnOut);
    pisoShiftRegister #(256) pri1(patPriority,clk,we,rst,patPriOut);
    pisoShiftRegister #(256) pa1(patPalette,clk,we,rst,patPalOut);
            
    bit shiftOut3,shiftOut2,shiftOut1,shiftOut0, fifthBit;  
    always_comb begin
        shiftOut3 = (patPriOut) ? (patEnOut ? patBit3 : sprBit3) : (sprEnOut ? sprBit3 : patBit3);
        shiftOut2 = (patPriOut) ? (patEnOut ? patBit2 : sprBit2) : (sprEnOut ? sprBit2 : patBit2);
        shiftOut1 = (patPriOut) ? (patEnOut ? patBit1 : sprBit1) : (sprEnOut ? sprBit1 : patBit1);
        shiftOut0 = (patPriOut) ? (patEnOut ? patBit0 : sprBit0) : (sprEnOut ? sprBit0 : patBit0);
        
        
        fifthBit = (patPriOut|!sprEnOut) ? patPalOut : 1'b1;
        
        enOut = patEnOut | sprEnOut;
        addr = enOut ? {fifthBit,shiftOut3, shiftOut2, shiftOut1, shiftOut0} : {fifthBit, backdropAddr}; 
    end
    
    always_ff @(posedge clk, posedge rst)
        if (rst) currS <= Wait;
        else currS <= nextS;
    
endmodule: outputSprites
    
module pisoShiftRegister //parallel in serial out
    #(parameter WIDTH = 6)
    (input logic [WIDTH-1:0] inVal,
    input logic clk, we, rst,
    output logic outVal);
    
    logic [WIDTH-1:0] shiftyVal; 
    
    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            shiftyVal <= 'b0;
            outVal <= 1'b0;
        end
        else if (we) begin
            shiftyVal <= inVal<<1;
            outVal <= inVal[WIDTH-1];
        end
        else begin
            outVal <= shiftyVal[WIDTH-1];
            shiftyVal <= shiftyVal<<1;
        end 
    end
    
endmodule: pisoShiftRegister
    
module hexToSevenSegment
  (output logic [6:0] segments,
   input  logic [3:0] value);

  always_comb begin
         case(value)
                4'h0: segments = 7'b100_0000;
                4'h1: segments = 7'b111_1001;
                4'h2: segments = 7'b010_0100;
                4'h3: segments = 7'b011_0000;
                4'h4: segments = 7'b001_1001;
                4'h5: segments = 7'b001_0010;
                4'h6: segments = 7'b000_0010;
                4'h7: segments = 7'b111_1000;
                4'h8: segments = 7'b000_0000;
                4'h9: segments = 7'b001_1000;
                4'hA: segments = 7'b000_1000;
                4'hB: segments = 7'b000_0011;
                4'hC: segments = 7'b100_0110;
                4'hD: segments = 7'b010_0001;
                4'hE: segments = 7'b000_0110;
                4'hF: segments = 7'b000_1110;
         endcase
  end

endmodule: hexToSevenSegment

    
    
    
    
    
    
    
    
    
    
    

