module z80_controller
  (output logic csr_l,
   output logic csw_l,
   output logic mode,
   output logic psgEn,
   output logic BUSACK_l,
   output logic [7:0] debug_val,
   output logic [15:0] address,
   output logic [7:0] vdpOutput,
   input  logic int_l,
   input logic nmi_l,
   input logic rst_l,
   input logic [5:0] controller_1,
   input logic [5:0] controller_2,
   input logic clk,
   input logic BUSREQ_l,
   input logic [7:0] vdpInput);

  logic [15:0] bf_command;
  logic [15:0] address_1, address_2, address_3, address_4;
  logic [17:0] bank_zero_address, bank_one_address, bank_two_address, cartridge_address;
  //wire [15:0] address;
  wire [7:0] data;
  bit [7:0] data_driven, be_data, bf_data, vram_output, frame_two_data;
  logic [7:0] if_data, ram_in_WR, rd_data, io_rd_data, int_data,
              vram_count, ram_in, ram_out, ram_out_mirror,
              bios_out, card_out, cartridge_out, init_rom_out, memory_out, out_oriented;
  logic [7:0] bank_zero, bank_one, bank_two, frame_two_control;
  bit MEM_RQ_l, WR_l, RD_l, WAIT_l, RFSH_l, M1_l, IO_RQ_l, HALT_l;
  bit firstBiosInstruction;
  logic writeVDP, readVDP,
        vram_w, vram_r, cram, vdp_w,
        mem_a_en, 
        startIF, ifDone, WAIT_l_IF,
        startWR,
        startRD,
        MEM_WR_done, WAIT_l_WR, mem_a_en_WR, 
        MEM_RD_done, WAIT_l_RD, 
        startIO_RD, WAIT_l_IO_RD, IO_RD_done,
        startIO_WR, WAIT_l_IO_WR, IO_WR_done,
        acknowledge_int, interrupt_requested, int_done, WAIT_l_int, interrupt_switch,
        startNMI, nmi_done,
        drive_data, not_first_cycle,
        update_mem_enables, enable_ram, enable_bios_rom, enable_card_rom, enable_cartridge_rom, enable_init_rom,
        use_ram, use_mirror_ram, vdpStatus, useFrameTwoRam;

      enum logic [1:0] {
                        PRE_BIOS=2'b00,
                        BIOS=2'b01} consoleState, consoleNS;
      enum logic [2:0] {
                        BF_WAIT=3'b000,
                        BF_BYTE1=3'b001,
                        BF_POST_BYTE1=3'b010,
                        BF_BYTE2=3'b011,
                        VRAM_WRITE=3'b100,
                        VRAM_READ=3'b101,
                        CRAM=3'b110,
                        VDPSTATUS=3'b111} bfState, bfNS;
      enum logic [3:0] {
                        Z80_WAIT=4'b0000, 
                        IF=4'b0001, 
                        MEM_WR=4'b0010, 
                        MEM_RD=4'b0011, 
                        IO_RD=4'b0100,
                        IO_WR = 4'b1001,
                        BUS=4'b0101, 
                        INT=4'b0110, 
                        NMI=4'b0111,
                        HALT=4'b1000,
                        INT_SWITCH=4'b1010} z80State, z80NS;
      enum logic [2:0] {IF_T1=3'b000, IF_T2=3'b001, IF_T3=3'b010, IF_T4=3'b011, IF_INT_T1=3'b100, IF_INT_T2=3'b101} ifState, ifNS;
      enum logic [1:0] {MEM_WR_T1=2'b00, MEM_WR_T2=2'b01, MEM_WR_T3=2'b10} MEM_WRState, MEM_WRNS;
      enum logic [1:0] {MEM_RD_T1=2'b00, MEM_RD_T2=2'b01, MEM_RD_T3=2'b10} MEM_RDState, MEM_RDNS;
      enum logic [1:0] {IO_RD_T1=2'b00, IO_RD_T2=2'b01, IO_RD_TW=2'b10, IO_RD_T3=2'b11} IO_RDState, IO_RDNS;
      enum logic [1:0] {IO_WR_T1=2'b00, IO_WR_T2=2'b01, IO_WR_TW=2'b10, IO_WR_T3=2'b11} IO_WRState, IO_WRNS;
      enum logic [2:0] {INT_T1=3'b000, INT_T2=3'b001, INT_TW1=3'b010, INT_TW2=3'b101, INT_T3=3'b011, INT_T4=3'b100} intState, intNS;
      enum logic [1:0] {NMI_T1=2'b00, NMI_T2=2'b01, NMI_T3=2'b10, NMI_T4=2'b11} nmiState, nmiNS;

  assign vdpOutput = out_oriented;

  assign update_mem_enables = startIO_WR && ((address & 16'h00ff) == 16'h003e);
  assign use_ram = ((16'hc000 <= address) && (address < 16'he000));
  assign use_mirror_ram = ((16'he000 <= address) && (address < 16'hfffc));
  


*/
  registerZ80 #(1) first_cycle(.data_out(not_first_cycle), .we(~not_first_cycle), .rst_l, .clk, .data_in(WR_l));
  init_enable_register enable_ram_reg(.data_out(enable_ram), .we(update_mem_enables), .rst_l, .clk, .data_in(~data[4]));
  init_enable_register enable_bios_rom_reg(.data_out(enable_bios_rom), .we(update_mem_enables), .rst_l, .clk, .data_in(~data[3]));
  registerZ80 #(1) enable_card_rom_reg(.data_out(enable_card_rom), .we(update_mem_enables && (consoleState != PRE_BIOS)), .rst_l, .clk, .data_in(~data[5]));
  registerZ80 #(1) enable_cartridge_rom_reg(.data_out(enable_cartridge_rom), .we(update_mem_enables && (consoleState != PRE_BIOS)), .rst_l, .clk, .data_in(~data[6]));
  init_enable_register enable_init_rom_reg(.data_out(enable_init_rom), .we(update_mem_enables && enable_init_rom), .rst_l, .clk, .data_in(data[1]));
  init_enable_register firstBiosInstructionReg(.data_out(firstBiosInstruction), .we(consoleState == BIOS && ifDone), .rst_l, .clk, .data_in(1'b0));

  registerZ80 #(8) (.data_out(frame_two_control), .we(/**enable_ram &&*/ mem_a_en && (address == 16'hfffc)), .rst_l, .clk, .data_in(data));
  registerZ80 #(8) bank_zero_reg(.data_out(bank_zero), .we(/**enable_ram &&*/ mem_a_en && (address == 16'hfffd)), .rst_l, .clk, .data_in(data));
  bank_one_register bank_one_reg(.data_out(bank_one), .we(/**enable_ram &&*/ mem_a_en && (address == 16'hfffe)), .rst_l, .clk, .data_in(data));
  registerZ80 #(8) bank_two_reg(.data_out(bank_two), .we(/**enable_ram &&*/ mem_a_en && (address == 16'hffff)), .rst_l, .clk, .data_in(data));

  assign debug_val = (writeVDP) ? out_oriented : 8'bz;
  assign data = drive_data ? data_driven : 8'bz;
  
  assign cartridge_address = ((16'h0 <= address) && (address < 16'h4000)) ? bank_zero_address : 
                             ((16'h4000 <= address) && (address < 16'h8000)) ? bank_one_address :
                             ((16'h8000 <= address) && (address < 16'hc000)) ? bank_two_address :
                             18'h0;
                             
  assign bank_zero_address = ((address < 16'h0400) || (bank_zero == 4'b0000)) ? {4'b0000, address[13:0]} :
                              {bank_zero[3:0], address[13:0]};                           
  
  assign bank_one_address = {bank_one[3:0], address[13:0]};
  
  assign bank_two_address = {bank_two[3:0], address[13:0]};

  assign useFrameTwoRam = frame_two_control[3] && (16'h8000 <= address) && (address < 16'hc000) && mem_a_en;

  blk_mem_gen_0 z80_ram(.clka(clk), .ena(enable_ram), .wea(mem_a_en), .addra(address[12:0]), .dina(ram_in), .douta(ram_out));
  blk_mem_gen_0 z80_ram_mirror(.clka(clk), .ena(enable_ram), .wea(mem_a_en), .addra(address[12:0]), .dina(ram_in), .douta(ram_out_mirror));
  blk_mem_gen_1 bios_rom(.clka(clk), .ena(enable_bios_rom), .addra(address[12:0]), .douta(bios_out));
  blk_mem_gen_2 frame_ram(.clka(clk), .ena(enable_ram), .wea(useFrameTwoRam), .addra({frame_two_control[2], address[12:0]}), .dina(ram_in), .douta(frame_two_data));
  card_rom card_rom(.clka(clk), .ena(enable_card_rom), .addra(address[14:0]), .douta(card_out));
  //sonic_rom sonic_rom(.clka(clk), .ena(enable_cartridge_rom), .addra(cartridge_address), .douta(cartridge_out));
  init_rom initilize_rom(.clka(clk),.ena(enable_init_rom), .addra(address[4:0]), .douta(init_rom_out));

  z80_top_direct_n z80_chip(
    .nM1(M1_l),
    .nMREQ(MEM_RQ_l),
    .nIORQ(IO_RQ_l),
    .nRD(RD_l),
    .nWR(WR_l),
    .nRFSH(RFSH_l),
    .nHALT(HALT_l),
    .nBUSACK(BUSACK_l),
    .nWAIT(WAIT_l),
    .nINT(int_l || (consoleState == PRE_BIOS)),
    .nNMI(nmi_l),
    .nRESET(rst_l),
    .nBUSRQ(BUSREQ_l),
    .CLK(clk),
    .A(address),
    .D(data));

  always_ff@(posedge clk, negedge rst_l) begin
    if(~rst_l) begin
      bfState <= BF_WAIT;
      z80State <= Z80_WAIT;
      ifState <= IF_T1;
      MEM_WRState <= MEM_WR_T1;
      MEM_RDState <= MEM_RD_T1;
      IO_RDState <= IO_RD_T2;
      IO_WRState <= IO_WR_T2;
      intState <= INT_T1;
      nmiState <= NMI_T1;
      consoleState <= PRE_BIOS;
      //consoleState <= BIOS;
    end
    else if (WR_l || not_first_cycle) begin
      bfState <= bfNS;
      z80State <= z80NS;
      ifState <= ifNS;
      MEM_WRState <= MEM_WRNS;
      MEM_RDState <= MEM_RDNS;
      IO_RDState <= IO_RDNS;
      IO_WRState <= IO_WRNS;
      intState <= intNS;
      nmiState <= nmiNS;
      consoleState <= consoleNS;
    end
  end

  always_comb begin
    startIF = 1'b0;
    startWR = 1'b0;
    startRD = 1'b0;
    startIO_RD = 1'b0;
    startIO_WR = 1'b0;
    acknowledge_int = 1'b0;
    startNMI = 1'b0;
    WAIT_l = 1'b1;
    mem_a_en = 1'b0;
    data_driven = 8'bz;
    drive_data = 1'b0;
    ram_in = 8'b0;
    out_oriented = 8'bz;
    case(z80State)
      Z80_WAIT: begin
        startIF = ~MEM_RQ_l && ~M1_l && ~RD_l;
        startWR = ~MEM_RQ_l && RD_l;
        startRD = ~MEM_RQ_l && ~RD_l;
        startIO_RD = ~IO_RQ_l && ~RD_l;
        startIO_WR = ~IO_RQ_l && ~WR_l;
        acknowledge_int = MEM_RQ_l && ~M1_l && RD_l;
        startNMI = MEM_RQ_l && ~M1_l && ~RD_l;
        z80NS = (startIF) ? IF :
                (startWR) ? MEM_WR : 
                (startRD) ? MEM_RD :
                (startIO_WR) ? IO_WR : 
                (startIO_RD) ? IO_RD : 
                (~BUSREQ_l) ? BUS : 
                (acknowledge_int) ? INT :
                (startNMI) ? NMI : 
                /**(~HALT_l) ? HALT :*/ Z80_WAIT;
      end
      IF: begin
        WAIT_l = WAIT_l_IF;
        data_driven = if_data;
        drive_data = 1'b1;
        z80NS = (ifDone) ? Z80_WAIT : IF;
      end
      MEM_WR: begin
        WAIT_l = WAIT_l_WR;
        mem_a_en = mem_a_en_WR;
        ram_in = ram_in_WR;
        z80NS = (MEM_WR_done) ? Z80_WAIT : MEM_WR;
      end
      MEM_RD: begin
        WAIT_l = WAIT_l_RD;
        data_driven = rd_data;
        drive_data = 1'b1;
        z80NS = (MEM_RD_done) ? Z80_WAIT : MEM_RD;
      end
      IO_WR: begin
        WAIT_l = WAIT_l_IO_WR;
        z80NS = (IO_WR_done) ? Z80_WAIT : IO_WR;
        out_oriented = data;//(writeVDP) ? data : 8'bz;
      end
      IO_RD: begin
        WAIT_l = WAIT_l_IO_RD;
        data_driven = io_rd_data;
        drive_data = 1'b1;
        z80NS = (IO_RD_done) ? Z80_WAIT : IO_RD;
      end
      BUS: begin
        z80NS = (BUSACK_l) ? Z80_WAIT : BUS;
      end
      INT: begin
        z80NS = (int_done) ? INT_SWITCH : INT; 
        //in the sense done switching to interrupt handler, not actually done with the interrupt
        WAIT_l = WAIT_l_int;
        //data_driven = int_data;
        //drive_data = 1'b1;
      end
      NMI: begin
        z80NS = (nmi_done) ? INT_SWITCH : NMI;
      end
      HALT: begin
        z80NS = (HALT_l) ? Z80_WAIT : HALT;
      end
      INT_SWITCH: begin
        startIF = ~MEM_RQ_l && ~M1_l;
        mem_a_en = ~MEM_RQ_l && ~WR_l;
        ram_in = data;
        z80NS = (startIF) ? IF : INT_SWITCH;
      end
    endcase
  end
  
  always_comb begin
    ifDone = 1'b0;
    WAIT_l_IF = 1'b1;
    if_data = 8'bz;
    case(ifState)
      IF_T1: begin
        ifNS = (startIF) ? IF_T2 : IF_T1;
      end
      IF_T2: begin
        ifNS = (~MEM_RQ_l) ? IF_T3 : IF_INT_T1;
        //WAIT_l_IF = 1'b0;
        if_data = memory_out;
      end
      IF_INT_T1: begin
        ifNS = IF_INT_T2;      
      end
      IF_INT_T2: begin
        ifNS = IF_T3;      
      end
      IF_T3: begin
        ifNS = IF_T4;
      end
      IF_T4: begin
        ifNS = IF_T1;
        ifDone = 1'b1;
      end
    endcase
  end
  
  always_comb begin
    MEM_WR_done = 1'b0;
    WAIT_l_WR = 1'b1;
    mem_a_en_WR = 1'b0;
    ram_in_WR = 8'b0;
    case(MEM_WRState)
      MEM_WR_T1: begin
        MEM_WRNS = (startWR) ? MEM_WR_T2 : MEM_WR_T1;
      end
      MEM_WR_T2: begin
        MEM_WRNS = MEM_WR_T3;
        WAIT_l_WR = 1'b0;
      end
      MEM_WR_T3: begin
        MEM_WRNS = MEM_WR_T1;
        MEM_WR_done = 1'b1;
        mem_a_en_WR = 1'b1;
        ram_in_WR = data;
      end
    endcase
  end
  
  always_comb begin
    MEM_RD_done = 1'b0;
    rd_data = 8'bz;
    WAIT_l_RD = 1'b1;
    case(MEM_RDState)
      MEM_RD_T1: begin
        MEM_RDNS = (startRD) ? MEM_RD_T2 : MEM_RD_T1;
      end
      MEM_RD_T2: begin
        MEM_RDNS = MEM_RD_T3;
        WAIT_l_RD = 1'b0;
      end
      MEM_RD_T3: begin
        MEM_RDNS = MEM_RD_T1;
        MEM_RD_done = 1'b1;
        rd_data =  memory_out;
      end
    endcase
  end
  always_comb begin
    IO_WR_done = 1'b0;
    WAIT_l_IO_WR = 1'b1;
    psgEn = 1'b0;
    case(IO_WRState)
      IO_WR_T1: begin
        IO_WRNS = (startIO_WR) ? IO_WR_T2 : IO_WR_T1;
      end
      IO_WR_T2:begin
        IO_WRNS = (startIO_WR) ? IO_WR_TW : IO_WR_T2;      
      end
      IO_WR_TW: begin
        IO_WRNS = IO_WR_T3;
        WAIT_l_IO_WR = 1'b0;
        psgEn = (address[7:0] == 8'h7f);
      end
      IO_WR_T3: begin
        IO_WRNS = IO_WR_T2;
        IO_WR_done = 1'b1;
      end
    endcase  
  end
  
  always_comb begin
    IO_RD_done = 1'b0;
    WAIT_l_IO_RD = 1'b1;
    io_rd_data = 8'bz;
    vdpStatus = 1'b0;
    case(IO_RDState)
      IO_RD_T1: begin
        IO_RDNS = (startIO_RD) ? IO_RD_T2 : IO_RD_T1;
      end
      IO_RD_T2: begin
        vdpStatus = startIO_RD && ((address & 16'h00ff) == 16'h00bf);
        IO_RDNS = (startIO_RD) ? IO_RD_TW : IO_RD_T2;
      end
      IO_RD_TW: begin
        IO_RDNS = IO_RD_T3;
        WAIT_l_IO_RD = 1'b0;
        //according to timing diagrams in z80 documentation, this should
        // be in t3, but I think its off by a cycle in the az80
        io_rd_data = ((address & 16'h00ff) == 16'h00dc) ? 
                  {
                    controller_2[1], 
                    controller_2[0],
                    controller_1[5],
                    controller_1[4],
                    controller_1[3],
                    controller_1[2],
                    controller_1[1],
                    controller_1[0] 
                  } : ((address & 16'h00ff) == 16'h00dd) ?
                  {
                    1'b0,
                    1'b0,
                    1'b0,
                    rst_l,
                    controller_2[5],
                    controller_2[4],
                    controller_2[3],
                    controller_2[2]
                  } : (address[7:0] == 8'hbf) ? vdpInput : 8'hb0;
      end
      IO_RD_T3: begin
        IO_RDNS = IO_RD_T2;
        IO_RD_done = 1'b1;
        /**
        CONTROLLER_UP 0
        CONTROLLER_DOWN 1
        CONTROLLER_LEFT 2
        CONTROLLER_RIGHT 3
        CONTROLLER_S1 4
        CONTROLLER_S2 5
        */
      end
    endcase
  end

  always_comb begin
    WAIT_l_int = 1'b1;
    int_done = 1'b0;
    int_data = 8'bz;
    case(intState)
      INT_T1: begin
        intNS = (acknowledge_int) ? INT_T2 : INT_T1;
      end
      INT_T2: begin
        intNS = INT_TW1;
      end
      INT_TW1: begin
        intNS = INT_TW2;
        //WAIT_l_int = 1'b0;
      end
      INT_TW2: begin
        intNS = INT_T3;
      end
      INT_T3: begin
        intNS = INT_T4;
      end
      INT_T4: begin
        intNS = INT_T1;
        int_done = 1'b1;
      end
    endcase
  end

  always_comb begin
    nmi_done = 1'b0;
    case(nmiState)
      NMI_T1: begin
        nmiNS = (startNMI) ? NMI_T2 : NMI_T1;
      end
      NMI_T2: begin
        nmiNS = NMI_T3;
      end
      NMI_T3: begin
        nmiNS = NMI_T4;
      end
      NMI_T4: begin
        nmiNS = NMI_T1;
        nmi_done = 1'b1;
      end
    endcase
  end

  always_comb begin
    mode = 1'b0;
    csw_l = 1'b1;
    csr_l = 1'b1;
    vdp_w = 1'b0;
    vram_r = 1'b0;
    vram_w = 1'b0;
    cram = 1'b0;
    writeVDP = (IO_WRState == IO_WR_TW) && 
      (((address & 16'h00ff) == 16'h00bf) || ((address & 16'h0ff) == 16'h00be));
    readVDP = (IO_RDState == IO_RD_TW) &&
      ((address & 16'h00ff) == 16'h00be);
    case(bfState)
      BF_WAIT: begin
        bfNS = (vdpStatus) ? VDPSTATUS : (writeVDP) ? BF_BYTE1 : BF_WAIT;
      end
      BF_BYTE1: begin
        bfNS = BF_POST_BYTE1;
        csw_l = 1'b0;
        mode = 1'b1;   
      end
      BF_POST_BYTE1: begin
        bfNS = (writeVDP) ? BF_BYTE2 : BF_POST_BYTE1;
      end
      BF_BYTE2: begin
        vdp_w = (data[7] && ~data[6] && ~data[5] && ~data[4]);
        vram_r = (~data[7] && ~data[6]);
        vram_w = (~data[7] && data[6]);
        cram = (data == 8'b1100_0000);
        mode = 1'b1;
        csw_l = 1'b0;
        csr_l = 1'b1;
        bfNS = (vram_r) ? VRAM_READ :
               (vram_w) ? VRAM_WRITE :
               (cram) ? CRAM :
               BF_WAIT;
      end
      VRAM_READ: begin
        csw_l = 1'b1;
        csr_l = ~readVDP;
        mode = 1'b0;
        bfNS = (vdpStatus) ? VDPSTATUS : (startIO_WR && ((address & 16'h00ff) == 16'h00bf)) ? BF_WAIT : VRAM_READ;
      end
      VRAM_WRITE: begin
        csw_l = ~writeVDP;
        csr_l = 1'b1;
        mode = 1'b0;
        bfNS = (vdpStatus) ? VDPSTATUS : (startIO_WR && ((address & 16'h00ff) == 16'h00bf)) ? BF_WAIT : VRAM_WRITE;
      end
      CRAM: begin
        csw_l = ~writeVDP;
        csr_l = 1'b1;
        mode = 1'b0;
        bfNS = (vdpStatus) ? VDPSTATUS : (startIO_WR && ((address & 16'h00ff) == 16'h00bf)) ? BF_WAIT : CRAM;
      end
      VDPSTATUS: begin
        csr_l = 1'b0;
        mode = 1'b1;
        bfNS = BF_WAIT;
      end
    endcase
  end
  always_comb begin
    memory_out = 8'b0;
    case(consoleState)
        PRE_BIOS: begin
            memory_out = (enable_init_rom && (address < 16'h2000)) ? init_rom_out : 8'he9; 
            consoleNS = (~enable_card_rom && ~enable_cartridge_rom && ~enable_init_rom) ? BIOS : PRE_BIOS;
        end
        BIOS: begin
          memory_out = (firstBiosInstruction) ? 8'he9 : 
                       (enable_bios_rom && (address < 16'h4000)) ? bios_out : 
                       (enable_ram && use_ram) ? ram_out : 
                       (enable_ram && use_mirror_ram) ? ram_out_mirror :
                       (enable_card_rom && (address < 16'h8000)) ? card_out :
                       (enable_cartridge_rom && (address < 16'hc000) && ~frame_two_control[3]) ? cartridge_out :
                       (enable_ram && useFrameTwoRam) ? frame_two_data :
                       (enable_ram && (address == 16'hfffd)) ? bank_zero :
                       (enable_ram && (address == 16'hfffe)) ? bank_one :
                       (enable_ram && (address == 16'hffff)) ? bank_two :
                       8'he9;
          consoleNS = BIOS;
        end
    endcase
  end
endmodule: z80_controller

module registerZ80
  #(parameter WIDTH = 8)
  (output logic [WIDTH-1:0] data_out,
   input logic we, 
   input logic rst_l,
   input logic clk,
   input logic [WIDTH-1:0] data_in);

   always_ff @ (posedge clk, negedge rst_l) begin
     if (~rst_l) data_out <= 0;
     else if (we) data_out <= data_in;
   end

endmodule: registerZ80

module init_enable_register
  (output logic data_out,
   input logic we, 
   input logic rst_l,
   input logic clk,
   input logic data_in);

   always_ff @ (posedge clk, negedge rst_l) begin
     if (~rst_l) data_out <= 1'b1;
     else if (we) data_out <= data_in;
   end

endmodule: init_enable_register

module bank_one_register
  (output logic [7:0] data_out,
   input logic we, 
   input logic rst_l,
   input logic clk,
   input logic [7:0] data_in);

   always_ff @ (posedge clk, negedge rst_l) begin
     if (~rst_l) data_out <= 8'h01;
     else if (we) data_out <= data_in;
   end

endmodule: bank_one_register

module fffc_register
  (output logic [3:0] data_out,
   input logic we, 
   input logic rst_l,
   input logic clk,
   input logic [3:0] data_in);

   always_ff @ (posedge clk, negedge rst_l) begin
     if (~rst_l) data_out <= 4'b0000;
     else if (we) data_out <= data_in;
   end

endmodule: fffc_register

module counterZ80
  #(parameter WIDTH=8)
  (output logic [WIDTH-1:0] data_out,
   input logic clr,
   input logic en,
   input logic rst_l,
   input logic clk);

  always_ff @ (posedge clk, negedge rst_l) begin
    if(~rst_l) data_out <= 0;
    else if (clr) data_out <= 0;
    else if (en) data_out <= data_out + 1;
  end

endmodule: counterZ80
