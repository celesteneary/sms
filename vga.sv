`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: Swag Master System
// Engineer: 
// 
// Create Date: 09/10/2016 04:15:00 PM
// Design Name: 
// Module Name: lab1
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module vga_wrapper(
  input logic clk,
  input logic btnCpuReset,
  input bit [7:0] pramOut,
  output bit [9:0] row2vdp, col2vdp,
  output logic [3:0] vgaRed, vgaGreen, vgaBlue, 
  output logic       Vsync, Hsync,
  output logic hLineInt, vBlankInt,
  input logic [7:0] hli, //hline check thing
  input logic [7:0] vScrollVal, hScrollVal,
  input logic vSInhib, hSInhib,
  output logic [9:0] vgaRow); //scroll inhibits
  
    logic blank;
    logic HS, VS;
    logic newclk;
    
    logic [9:0] row,col;
  
  /* begin interrupt handling */
  bit [7:0] count1;
  bit underflow, hliEn, hliClr;
  countdown #(8) hlicd(count1, newclk, hliEn, hliClr, ~btnCpuReset, hli);
  assign underflow = (count1 == 1'b0);
  
  assign hLineInt = underflow&&(col==10'd578);
  assign hliEn = (col==10'd576)&&(row2vdp!= 9'h100); //finished a real line
  assign hliClr = (row==10'd440); //all the way down
  assign vBlankInt =  (row==10'd440)&&(col==10'd576); //just output for one cycle
  
  /* end interrupt handling */
  

  vga v(.CLOCK_50(newclk), .reset(~btnCpuReset), .HS(Hsync), .VS(Vsync), .blank(blank), .row(row), .col(col));
  
  //100 MHz to 50MHz clock
  divide_clock_by_two div(.in_clk(clk), .out_clk(newclk), .rst(~btnCpuReset));

  logic [9:0] temp1, temp2;
  
  logic rowBetween, colBetween;
  always_comb begin
    if (blank || !rowBetween || !colBetween) begin
        vgaRed = 4'b0000;
        vgaGreen = 4'b0000;
        vgaBlue = 4'b0000;
    end
    else begin
        vgaRed = {pramOut[1:0], pramOut[1:0]};
        vgaGreen = {pramOut[3:2], pramOut[3:2]};
        vgaBlue = {pramOut[5:4], pramOut[5:4]};
    end
  end
  
  assign temp1 = row-10'd48;
  assign temp2 = col-10'd64;
  
  //handle scrolling - where we read from pram
  logic [8:0] displayRow, displayCol;
  logic [8:0] pramRow, pramCol;
  always_comb begin
    displayRow = (!rowBetween) ? 9'h100 : temp1[9:1];
    displayCol = (!colBetween) ? 9'h100 : temp2[9:1];
    
    col2vdp = displayCol;
    row2vdp = displayRow;
    
  end
  
  range_check rowCheck(row, 48, 431,rowBetween),
              colCheck(col, 64, 575,colBetween);
    
endmodule: vga_wrapper


module vga ( //pixel freq 25 MHz not 50
    output logic [9:0] row, col,
    output logic       HS, VS, blank,
    input  logic       CLOCK_50, reset
    );

    logic [10:0] col_count;
    logic        col_clear, col_enable;
    logic [9:0]  row_count;
    logic        row_clear, row_enable;
    logic        h_blank, v_blank;

    // Row counter counts from 0 to 520
    //     count of   0 - 479 is display time (thus row_count is correct here)
    //     count of 480 - 489 is front porch
    //     count of 490 - 491 is VS=0 pulse width
    //     count of 492 - 520 is back porch

    simple_counter #(10) row_counter(
            .Q      (row_count),
            .en     (row_enable),
            .clr    (row_clear),
            .clk    (CLOCK_50),
            .reset  (reset)
            );

    assign row        = row_count;
    assign row_clear  = (row_count >= 10'd520);
    assign row_enable = (col_count == 11'd1599);
    assign VS         = (row_count < 10'd490) | (row_count > 10'd491);
    assign v_blank    = (row_count >= 10'd480);

    // Col counter counts from 0 to 1599
    //     count of    0 - 1279 is display time (col is div by 2)
    //     count of 1280 - 1311 is front porch
    //     count of 1312 - 1503 is HS=0 pulse width
    //     count of 1504 - 1599 is back porch

    simple_counter #(11) col_counter(
            .Q      (col_count),
            .en     (col_enable),
            .clr    (col_clear),
            .clk    (CLOCK_50),
            .reset  (reset)
            );

    assign col        = col_count[10:1];
    assign col_clear  = (col_count >= 11'd1599);
    assign col_enable = 1'b1;
    assign HS         = (col_count < 11'd1312) | (col_count > 11'd1503);
    assign h_blank    = col_count > 11'd1279;

    assign blank      = h_blank | v_blank;
endmodule: vga

/*****************************************************************
 *
 *                    Library modules
 *
 *****************************************************************/

/** BRIEF
 *  Outputs whether a value lies between [low, high].
 */
module range_check
    #(parameter WIDTH = 4'd10) (
    input  logic [WIDTH-1:0] val, low, high,
    output logic             is_between
    );

    assign is_between = (val >= low) & (val <= high);

endmodule: range_check

/** BRIEF
 *  Outputs whether a value lies between [low, low + delta].
 */
module offset_check
    #(parameter WIDTH = 4'd10) (
    input  logic [WIDTH-1:0] val, low, delta,
    output logic             is_between
    );

    assign is_between = ((val >= low) & (val < (low+delta)));

endmodule: offset_check

/** BRIEF
 *  Simple up counter with synchronous clear and enable.
 *  Clear takes precedence over enable.
 */
module simple_counter
    #(parameter WIDTH = 8) (
    output logic [WIDTH-1:0] Q,
    input  logic             clk, en, clr, reset
    );

    always_ff @(posedge clk, posedge reset)
        if (reset)
            Q <= 'b0;
        else if (clr)
            Q <= 'b0;
        else if (en)
            Q <= (Q + 1'b1);

endmodule: simple_counter

/** BRIEF
 *  Simple down counter with synchronous clear and enable.
 *  Clear takes precedence over enable.
 */
module countdown
    #(parameter WIDTH = 8) (
    output bit [WIDTH-1:0] Q,
    input  logic clk, en, clr, reset, 
    input logic [WIDTH-1:0] val
    );

    always_ff @(posedge clk, posedge reset)
        if (reset)
            Q <= val;
        else if (clr)
            Q <= val;
        else if (en)
            Q <= (Q==0) ? val : (Q - 1'b1);


endmodule: countdown























