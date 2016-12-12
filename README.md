# sms
Sega Master System in SystemVerilog
Created by Celeste Neary, Suzz Glennon, and Jeremy Sonpar for 18-545, taught by Brandon Lucia at Carnegie Mellon University Fall 2016.
Included is all of the source code necessary to create a Vivado project and program a Sega Master System on a Nexys 4 board.

SMSFinalReport.pdf is our full Sega Master System project report.

top.sv is the top module for the project. The inputs and outputs are based on the Nexys 4 XDC file.

z80_controller.sv is the interface between the Z80 processor and the rest of the Master System. It was designed for use with the A-Z80
processor by Goran Devic (found on OpenCores).

Our implementation of the Video Display Processor (VDP) includes the following files:
vdp.sv, patternCollector.sv, spriteCollector.sv, vga.sv, and lib.sv

psg.sv is the Programmable Sound Generator (PSG)

coeGen.py is a script we used to generate coefficient files for Block RAM from game ROMs
