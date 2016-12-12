/* psg.sv
 * Programmable Sound Generator
 * Sega Master System
 * Celeste Neary, Jeremy Sonpar, & Suzz Glennon
 */

module psg
  (output logic [15:0] led,
   output logic ampPWM, ampSD,
   output logic [1:0] JC,
   input  logic cpuClk, clk100, btnCpuReset,
   input  logic we_l, oe_l,
   input  logic [7:0] databus);
   
    //enable pwm output
    assign ampSD = 1'b1;

    logic [3:0] vol0, vol1, vol2, vol3;
    logic [9:0] tone0, tone1, tone2;
    logic [2:0] noise;
    logic we_l, oe_l;
    logic [7:0] databus;
    logic [10:0] regEn, nextEn;
    logic [6:0] dutyCycle;
    logic toneOut0, toneOut1, toneOut2;
    logic signal;

    assign led[10:0] = regEn;

    psgFSM psgFSM0(vol0, vol1, vol2, vol3, tone0, tone1, tone2, noise, regEn, nextEn,
             cpuClk, we_l, oe_l, ~btnCpuReset, databus);    
    
    toneMaker tm0(toneOut0, clk100, ~btnCpuReset, tone0);
    toneMaker tm1(toneOut1, clk100, ~btnCpuReset, tone1);
    toneMaker tm2(toneOut2, clk100, ~btnCpuReset, tone2);
    
    assign dutyCycle = (toneOut0 * (vol0)) + (toneOut1 * (vol1)) + (toneOut2 * (vol2)) + (1'b0 * (vol3)); //TODO: noise
    signalMaker sm0(signal, clk100, ~btnCpuReset, dutyCycle); 
    assign ampPWM = signal;
    
    assign JC = {toneOut0, signal};

endmodule: psg

//creates the each tone -- not PWM
module toneMaker
    (output logic toneOut,
     input  logic clk, rst,
     input  logic [9:0] tone);
     
     bit done, up;
     bit [19:0] cycles, currCycle;
     
     assign cycles = tone * 447;

    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            toneOut <= 1'b0;
            currCycle <= 20'b0;
        end
        else begin
            if ((currCycle == cycles) && (cycles != 20'b0)) begin
                currCycle <= 20'b0;
                toneOut <= ~toneOut;
            end
            else currCycle <= currCycle + 1'b1;
        end
    end
    
endmodule: toneMaker

//outputs a given duty cycle
module signalMaker
    (output logic signal,
     input  logic clk, rst,
     input  logic [6:0] dutyCycle);

    logic [6:0] count;

    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            count <= 7'b0;
            signal <= 1'b0;
        end
        else begin
            //signal output logic
            if (count < dutyCycle) signal <= 1'b1;
            else signal <= 1'b0;
            
            //done with 100 clock cycles?
            if (count == 100) count <= 7'b0; //reset count to 0          
            else count <= count + 1'b1;
        end
    end

endmodule: signalMaker

/* write to 0x7f */
module psgFSM
  (output logic [3:0] vol0, vol1, vol2, vol3,
   output logic [9:0] tone0, tone1, tone2, 
   output logic [2:0] noise,
   output logic [10:0] regEn, nextEn,
   input  logic clk, we_l, oe_l, rst,
   input  logic [7:0] databus);

  bit [3:0] volD, vol0_internal, vol1_internal, vol2_internal, vol3_internal;
  bit [9:0] whiteNoiseCounter;
  bit [5:0] toneDUpper, tone0U, tone1U, tone2U;
  bit [3:0] toneDLower, tone0L, tone1L, tone2L;
  bit [2:0] noiseD;
  bit [10:0] en;
  bit whiteNoiseRegEn;

  assign vol0 = (~oe_l) ? ~vol0_internal : 4'b0000;
  assign vol1 = (~oe_l) ? ~vol1_internal : 4'b0000;
  assign vol2 = (~oe_l) ? ~vol2_internal : 4'b0000;
  assign vol3 = (~oe_l) ? ~vol3_internal : 4'b0000;

  assign tone0 = {tone0U, tone0L};
  assign tone1 = {tone1U, tone1L};
  assign tone2 = {tone2U, tone2L};

  enum logic [1:0] {Start, Wait, LatchData, Data} currS, nextS;

  assign state = currS;

  always_ff @(posedge clk, posedge rst)
    if (rst) currS <= Start;
    else currS <= nextS;

  //next state logic
  assign nextS = (we_l) ? Wait : (databus[7] ? LatchData : Data);

  bit t;
  bit [1:0] c;
  bit [5:0] d;

  always_comb begin
    //latch/data
    if (currS == LatchData) begin
      c = databus[6:5];
      t = databus[4];
      d = {2'b0, databus[3:0]}; //only use [3:0] of d
    end
    //data, latch stays the same
    else begin
      c = 2'b0;
      t = 1'b0;
      d = databus[5:0];
    end
  end

    assign regEn = en;// & ((we_l) ? 11'b0 : 11'b111_1111_1111);

    //register enables and save databus values
    always_ff @(posedge clk, posedge rst) begin
        if (rst) begin
            en <= 11'b000_0000_0000;
            nextEn <= 11'b000_0000_0000;
            volD <= 4'b1111;
            toneDUpper <= 6'b0;
            toneDLower <= 4'b0;
            noiseD <= 3'b0;
            vol0_internal <= 4'b1111;
            vol1_internal <= 4'b1111;
            vol2_internal <= 4'b1111;
            vol3_internal <= 4'b1111;
            tone0L <= 4'b0;
            tone1L <= 4'b0;
            tone2L <= 4'b0;
            noise <= 3'b0;
            tone0U <= 6'b0;
            tone1U <= 6'b0;
            tone2U <= 6'b0;
        end
    else begin
        //load values
        volD <= d[3:0];
        toneDUpper <= d;
        toneDLower <= d[3:0];
        noiseD <= d[2:0];
        
        if (currS == LatchData) begin
          unique case ({t, c})
            //type = 0
            3'b000: begin
              en <= 11'b000_0001_0000; //enable tone0
              nextEn <= 11'b001_0000_0000; //upper bits of tone0
            end
            3'b001: begin
              en <= 11'b000_0010_0000; //enable tone1
              nextEn <= 11'b010_0000_0000; //upper bits of tone1
            end
            3'b010: begin
              en <= 11'b000_0100_0000; //enable tone2
              nextEn <= 11'b100_0000_0000; //upper bits of tone2
            end
            3'b011: begin
              en <= 11'b000_1000_0000; //enable noise
              nextEn <= 11'b000_1000_0000;
            end
    
            //type = 1
            3'b100: begin
              en <= 11'b000_0000_0001; //enable vol0
              nextEn <= 11'b000_0000_0001;
            end
            3'b101: begin
              en <= 11'b000_0000_0010; //enable vol1
              nextEn <= 11'b000_0000_0010;
            end
            3'b110: begin
              en <= 11'b000_0000_0100; //enable vol2
              nextEn <= 11'b000_0000_0100;
            end
            3'b111: begin
              en <= 11'b000_0000_1000; //enable vol3
              nextEn <= 11'b000_0000_1000;
            end
          endcase
        end
        else if (currS == Wait) en <= 11'b0;
        else en <= nextEn;
        
        //registers
        if (en[0]) vol0_internal <= volD;
        else if (en[1]) vol1_internal <= volD;
        else if (en[2]) vol2_internal <= volD;
        else if (en[3]) vol3_internal <= volD;
        else if (en[4]) tone0L <= toneDLower;
        else if (en[5]) tone1L <= toneDLower;
        else if (en[6]) tone2L <= toneDLower;
        else if (en[7]) noise <= noiseD;
        else if (en[8]) tone0U <= toneDUpper;
        else if (en[9]) tone1U <= toneDUpper;
        else if (en[10]) tone2U <= toneDUpper;
    end
  end

endmodule: psgFSM

/* register with default value */
module register2
  #(parameter WIDTH = 6)
  (input  logic [WIDTH-1:0] D, def,
   input  logic clk, rst, en,
   output logic [WIDTH-1:0] Q);

  always_ff @(posedge clk, posedge rst) begin
    if (rst) Q <= def;
    else if (en) Q <= D;
  end

endmodule: register2
