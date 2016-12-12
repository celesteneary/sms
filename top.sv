module top
    (input logic clk,
    input logic btnCpuReset,
    input logic [11:0] JA, JB, //controller inputs
    input logic btnU, btnD, btnL, btnR, 
    input logic [1:0] sw,
    output logic [1:0] JC,
    output logic [15:0] led,
    output logic [3:0] vgaRed, vgaGreen, vgaBlue, 
    output logic       Vsync, Hsync, ampPWM, ampSD);
 
    bit [15:0] address;
    logic [7:0] debug_val;
    wire [7:0] data;
    bit [5:0] controller_1, controller_2, controller1_temp, controller2_temp;
    bit clk7_16, cpuClk, clk100, clkLocked;
    bit csr_l, csw_l, mode, BUSACK_l, rw, int_l, nmi_l, rst_l, BUSREQ_l, psgEn;


    bit bCRst1, btnCpuResetOut;
    register #(1) debounce1(btnCpuReset, clk100&&clkLocked, 1'b0, 1'b1, bCRst1);
    register #(1) debounce2(bCRst1, clk100&&clkLocked, 1'b0, 1'b1, btnCpuResetOut);
  
    register #(6) controller1debounce1(.reset(~btnCpuResetOut), .clock(clk100&&clkLocked), .en(1'b1), .D(JA[5:0]), .Q(controller1_temp));
    register #(6) controller1debounce2(.reset(~btnCpuResetOut), .clock(clk100&&clkLocked), .en(1'b1), .D(controller1_temp), .Q(controller_1));

    register #(6) controller2debounce1(.reset(~btnCpuResetOut), .clock(clk100&&clkLocked), .en(1'b1), .D(JB[5:0]), .Q(controller2_temp));
    register #(6) controller2debounce2(.reset(~btnCpuResetOut), .clock(clk100&&clkLocked), .en(1'b1), .D(controller2_temp), .Q(controller_2));

    assign BUSREQ_l = 1'b1;

    //debugging signals
    logic [2:0] stateOutput;
    logic [7:0] dataOutVDP, dataOutZ80, cramOut;
    logic [7:0] spriteRow, patCol;
    logic [8:0] spriteCol, shift;
    logic       delayLineSave;
    logic enOut, fifthBit, patPalOut, shiftOut3, shiftOut2, shiftOut1, shiftOut0, patPriOut;
    logic [3:0] backdropAddr;
      
    /* get 3.58MHz clock */
    clk_wiz_0 clk100to7_16(.clk_in1(clk), .clk_out1(clk7_16), .clk_out2(clk100), .reset(~btnCpuReset), .locked(clkLocked));
    divide_clock_by_two clk7_16to3_58(.in_clk(clkLocked && clk7_16), .rst(~btnCpuReset), .out_clk(cpuClk));

    z80_controller z80(.csr_l(csr_l), 
        .csw_l(csw_l), 
        .mode(mode), 
        .BUSACK_l(BUSACK_l), 
        .debug_val(debug_val),
        .address(address),
        .rw(rw), //w = 0
        .int_l(int_l),
        .nmi_l(1'b1), //for now
        .rst_l(btnCpuResetOut),
        .psgEn(psgEn),
        .controller_1(controller_1),
        .controller_2(controller_2),
        .led(led_out),
        .clk(cpuClk),
        .BUSREQ_l(BUSREQ_l),
        .vdpInput(dataOutVDP),
        .vdpOutput(dataOutZ80));

    vdp vdp0(.vgaRed(vgaRed),
       .vgaGreen(vgaGreen),
       .vgaBlue(vgaBlue),
       .Hsync(Hsync), 
       .Vsync(Vsync),
       .int_l(int_l), // CPU interrupt output
       .rw(rw),       // VRAM write strobe (w = 0)
       .dataIn(dataOutZ80),
       .dataOut(dataOutVDP),
       .cpuClk(cpuClk),
       .clk100(clk100 && clkLocked),
       .btnCpuReset(btnCpuResetOut),
       .csr_l(csr_l),       // CPU-VDP read strobe
       .csw_l(csw_l),       // CPU-VDP write strobe
       .mode(mode),         // CPU interface mode select
       
       .stateOutput(stateOutput),
       .spriteRow(spriteRow),
       .spriteCol(spriteCol),
       .patCol(patCol),
       .shift(shift),
       .delayLineSave(delayLineSave),
       .backdropAddrOut(backdrop),
       .backdropColor(backdropColor));

    psg psg0(.ampPWM(ampPWM),
        .ampSD(ampSD),
        .cpuClk(cpuClk),
        .clk100(clk100),
        .btnCpuReset(btnCpuResetOut),
        .we_l(~psgEn),
        .oe_l(1'b0),
        .databus(dataOutZ80),
        .JC(JC));
            
endmodule: top