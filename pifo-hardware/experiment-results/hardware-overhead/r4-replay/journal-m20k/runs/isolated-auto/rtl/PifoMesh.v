module PifoMesh (
  input  wire          io_push_valid,
  output wire          io_push_ready,
  input  wire          io_push_payload_post,
  input  wire [2:0]    io_push_payload_engineId,
  input  wire [9:0]    io_push_payload_vPifoId,
  input  wire [12:0]   io_push_payload_flowId,
  input  wire [12:0]   io_push_payload_data,
  output wire          io_pop_valid,
  input  wire          io_pop_ready,
  output wire          io_pop_payload_post,
  output wire [2:0]    io_pop_payload_engineId,
  output wire [9:0]    io_pop_payload_vPifoId,
  output wire [12:0]   io_pop_payload_flowId,
  output wire [12:0]   io_pop_payload_data,
  input  wire          io_flush,
  output wire [14:0]   io_occupancy,
  output wire [14:0]   io_availability,
  input  wire          clk,
  input  wire          reset
);

  reg        [39:0]   logic_ram_spinal_port1;
  wire       [39:0]   _zz_logic_ram_port;
  reg                 _zz_1;
  wire                logic_ptr_doPush;
  wire                logic_ptr_doPop;
  wire                logic_ptr_full;
  wire                logic_ptr_empty;
  reg        [14:0]   logic_ptr_push;
  reg        [14:0]   logic_ptr_pop;
  wire       [14:0]   logic_ptr_occupancy;
  wire       [14:0]   logic_ptr_popOnIo;
  wire                when_Stream_l1455;
  reg                 logic_ptr_wentUp;
  wire                io_push_fire;
  wire                logic_push_onRam_write_valid;
  wire       [13:0]   logic_push_onRam_write_payload_address;
  wire                logic_push_onRam_write_payload_data_post;
  wire       [2:0]    logic_push_onRam_write_payload_data_engineId;
  wire       [9:0]    logic_push_onRam_write_payload_data_vPifoId;
  wire       [12:0]   logic_push_onRam_write_payload_data_flowId;
  wire       [12:0]   logic_push_onRam_write_payload_data_data;
  wire                logic_pop_addressGen_valid;
  reg                 logic_pop_addressGen_ready;
  wire       [13:0]   logic_pop_addressGen_payload;
  wire                logic_pop_addressGen_fire;
  wire                logic_pop_sync_readArbitation_valid;
  wire                logic_pop_sync_readArbitation_ready;
  wire       [13:0]   logic_pop_sync_readArbitation_payload;
  reg                 logic_pop_addressGen_rValid;
  reg        [13:0]   logic_pop_addressGen_rData;
  wire                when_Stream_l477;
  wire                logic_pop_sync_readPort_cmd_valid;
  wire       [13:0]   logic_pop_sync_readPort_cmd_payload;
  wire                logic_pop_sync_readPort_rsp_post;
  wire       [2:0]    logic_pop_sync_readPort_rsp_engineId;
  wire       [9:0]    logic_pop_sync_readPort_rsp_vPifoId;
  wire       [12:0]   logic_pop_sync_readPort_rsp_flowId;
  wire       [12:0]   logic_pop_sync_readPort_rsp_data;
  wire       [39:0]   _zz_logic_pop_sync_readPort_rsp_post;
  wire                logic_pop_addressGen_toFlowFire_valid;
  wire       [13:0]   logic_pop_addressGen_toFlowFire_payload;
  wire                logic_pop_sync_readArbitation_translated_valid;
  wire                logic_pop_sync_readArbitation_translated_ready;
  wire                logic_pop_sync_readArbitation_translated_payload_post;
  wire       [2:0]    logic_pop_sync_readArbitation_translated_payload_engineId;
  wire       [9:0]    logic_pop_sync_readArbitation_translated_payload_vPifoId;
  wire       [12:0]   logic_pop_sync_readArbitation_translated_payload_flowId;
  wire       [12:0]   logic_pop_sync_readArbitation_translated_payload_data;
  wire                logic_pop_sync_readArbitation_fire;
  reg        [14:0]   logic_pop_sync_popReg;
  reg [39:0] logic_ram [0:16383];

  assign _zz_logic_ram_port = {logic_push_onRam_write_payload_data_data,{logic_push_onRam_write_payload_data_flowId,{logic_push_onRam_write_payload_data_vPifoId,{logic_push_onRam_write_payload_data_engineId,logic_push_onRam_write_payload_data_post}}}};
  always @(posedge clk) begin
    if(_zz_1) begin
      logic_ram[logic_push_onRam_write_payload_address] <= _zz_logic_ram_port;
    end
  end

  always @(posedge clk) begin
    if(logic_pop_sync_readPort_cmd_valid) begin
      logic_ram_spinal_port1 <= logic_ram[logic_pop_sync_readPort_cmd_payload];
    end
  end

  always @(*) begin
    _zz_1 = 1'b0;
    if(logic_push_onRam_write_valid) begin
      _zz_1 = 1'b1;
    end
  end

  assign when_Stream_l1455 = (logic_ptr_doPush != logic_ptr_doPop);
  assign logic_ptr_full = (((logic_ptr_push ^ logic_ptr_popOnIo) ^ 15'h4000) == 15'h0);
  assign logic_ptr_empty = (logic_ptr_push == logic_ptr_pop);
  assign logic_ptr_occupancy = (logic_ptr_push - logic_ptr_popOnIo);
  assign io_push_ready = (! logic_ptr_full);
  assign io_push_fire = (io_push_valid && io_push_ready);
  assign logic_ptr_doPush = io_push_fire;
  assign logic_push_onRam_write_valid = io_push_fire;
  assign logic_push_onRam_write_payload_address = logic_ptr_push[13:0];
  assign logic_push_onRam_write_payload_data_post = io_push_payload_post;
  assign logic_push_onRam_write_payload_data_engineId = io_push_payload_engineId;
  assign logic_push_onRam_write_payload_data_vPifoId = io_push_payload_vPifoId;
  assign logic_push_onRam_write_payload_data_flowId = io_push_payload_flowId;
  assign logic_push_onRam_write_payload_data_data = io_push_payload_data;
  assign logic_pop_addressGen_valid = (! logic_ptr_empty);
  assign logic_pop_addressGen_payload = logic_ptr_pop[13:0];
  assign logic_pop_addressGen_fire = (logic_pop_addressGen_valid && logic_pop_addressGen_ready);
  assign logic_ptr_doPop = logic_pop_addressGen_fire;
  always @(*) begin
    logic_pop_addressGen_ready = logic_pop_sync_readArbitation_ready;
    if(when_Stream_l477) begin
      logic_pop_addressGen_ready = 1'b1;
    end
  end

  assign when_Stream_l477 = (! logic_pop_sync_readArbitation_valid);
  assign logic_pop_sync_readArbitation_valid = logic_pop_addressGen_rValid;
  assign logic_pop_sync_readArbitation_payload = logic_pop_addressGen_rData;
  assign _zz_logic_pop_sync_readPort_rsp_post = logic_ram_spinal_port1;
  assign logic_pop_sync_readPort_rsp_post = _zz_logic_pop_sync_readPort_rsp_post[0];
  assign logic_pop_sync_readPort_rsp_engineId = _zz_logic_pop_sync_readPort_rsp_post[3 : 1];
  assign logic_pop_sync_readPort_rsp_vPifoId = _zz_logic_pop_sync_readPort_rsp_post[13 : 4];
  assign logic_pop_sync_readPort_rsp_flowId = _zz_logic_pop_sync_readPort_rsp_post[26 : 14];
  assign logic_pop_sync_readPort_rsp_data = _zz_logic_pop_sync_readPort_rsp_post[39 : 27];
  assign logic_pop_addressGen_toFlowFire_valid = logic_pop_addressGen_fire;
  assign logic_pop_addressGen_toFlowFire_payload = logic_pop_addressGen_payload;
  assign logic_pop_sync_readPort_cmd_valid = logic_pop_addressGen_toFlowFire_valid;
  assign logic_pop_sync_readPort_cmd_payload = logic_pop_addressGen_toFlowFire_payload;
  assign logic_pop_sync_readArbitation_translated_valid = logic_pop_sync_readArbitation_valid;
  assign logic_pop_sync_readArbitation_ready = logic_pop_sync_readArbitation_translated_ready;
  assign logic_pop_sync_readArbitation_translated_payload_post = logic_pop_sync_readPort_rsp_post;
  assign logic_pop_sync_readArbitation_translated_payload_engineId = logic_pop_sync_readPort_rsp_engineId;
  assign logic_pop_sync_readArbitation_translated_payload_vPifoId = logic_pop_sync_readPort_rsp_vPifoId;
  assign logic_pop_sync_readArbitation_translated_payload_flowId = logic_pop_sync_readPort_rsp_flowId;
  assign logic_pop_sync_readArbitation_translated_payload_data = logic_pop_sync_readPort_rsp_data;
  assign io_pop_valid = logic_pop_sync_readArbitation_translated_valid;
  assign logic_pop_sync_readArbitation_translated_ready = io_pop_ready;
  assign io_pop_payload_post = logic_pop_sync_readArbitation_translated_payload_post;
  assign io_pop_payload_engineId = logic_pop_sync_readArbitation_translated_payload_engineId;
  assign io_pop_payload_vPifoId = logic_pop_sync_readArbitation_translated_payload_vPifoId;
  assign io_pop_payload_flowId = logic_pop_sync_readArbitation_translated_payload_flowId;
  assign io_pop_payload_data = logic_pop_sync_readArbitation_translated_payload_data;
  assign logic_pop_sync_readArbitation_fire = (logic_pop_sync_readArbitation_valid && logic_pop_sync_readArbitation_ready);
  assign logic_ptr_popOnIo = logic_pop_sync_popReg;
  assign io_occupancy = logic_ptr_occupancy;
  assign io_availability = (15'h4000 - logic_ptr_occupancy);
  always @(posedge clk or posedge reset) begin
    if(reset) begin
      logic_ptr_push <= 15'h0;
      logic_ptr_pop <= 15'h0;
      logic_ptr_wentUp <= 1'b0;
      logic_pop_addressGen_rValid <= 1'b0;
      logic_pop_sync_popReg <= 15'h0;
    end else begin
      if(when_Stream_l1455) begin
        logic_ptr_wentUp <= logic_ptr_doPush;
      end
      if(io_flush) begin
        logic_ptr_wentUp <= 1'b0;
      end
      if(logic_ptr_doPush) begin
        logic_ptr_push <= (logic_ptr_push + 15'h0001);
      end
      if(logic_ptr_doPop) begin
        logic_ptr_pop <= (logic_ptr_pop + 15'h0001);
      end
      if(io_flush) begin
        logic_ptr_push <= 15'h0;
        logic_ptr_pop <= 15'h0;
      end
      if(logic_pop_addressGen_ready) begin
        logic_pop_addressGen_rValid <= logic_pop_addressGen_valid;
      end
      if(io_flush) begin
        logic_pop_addressGen_rValid <= 1'b0;
      end
      if(logic_pop_sync_readArbitation_fire) begin
        logic_pop_sync_popReg <= logic_ptr_pop;
      end
      if(io_flush) begin
        logic_pop_sync_popReg <= 15'h0;
      end
    end
  end

  always @(posedge clk) begin
    if(logic_pop_addressGen_ready) begin
      logic_pop_addressGen_rData <= logic_pop_addressGen_payload;
    end
  end


endmodule
