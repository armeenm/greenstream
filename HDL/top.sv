module top(
    input  logic         sysclk, // 12MHz
    input  logic         btn_rst,
    input  logic         btn_start,
    input  logic         btn_read,

    output logic  [16:0] addr,
    output logic         nce,
    output logic         noe,
    output logic         nwe,
    output logic         led_start,
    output logic         led_rst,
    output logic         nled_r,
    output logic         nled_g,
    output logic         nled_b,

    inout  logic  [7:0]  data_io
);

// Clock //
logic clk; // 7MHz
clk_wiz_0 clk_wiz(
    .clk_in  (sysclk),
    .clk_out (clk)
);

// FIFO //
logic       fifo_full, fifo_wr_en, fifo_empty, fifo_rd_en;
logic [7:0] fifo_din, fifo_dout;
fifo_generator_0 fifo(
    .srst        (rst),
    .clk         (clk),

    .full        (fifo_full),
    .din         (fifo_din),
    .wr_en       (fifo_wr_en),

    .empty       (fifo_empty),
    .dout        (fifo_dout),
    .rd_en       (fifo_rd_en)
);

// UART //
uart uart(
    .clk   (clk),
    .rst   (rst),
    
    .full  (fifo_full),
    
    .data  (fifo_din),
    .wr_en (fifo_wr_en)
);

// State //
typedef enum logic [5:0] {IDLE, SDP, WRITE, WAIT, CHECK, READ, ERROR} writer_t;
writer_t writer_state;

// Reset //
logic [3:0] rst_cnt = '0;
logic       int_rst  = 1;
logic       rst;
assign rst = btn_rst | int_rst;

// LED //
logic [2:0] led_rgb;
assign nled_r    = ~led_rgb[2];
assign nled_g    = ~led_rgb[1];
assign nled_b    = ~led_rgb[0];
assign led_rst   = rst;
assign led_start = btn_start;

// SDP //
logic [1:0] sdp_cnt = '0;
shortint addr_sdp[3] = '{'h5555, 'h2AAA, 'h5555};
byte data_sdp[3] = '{'hAA, 'h55, 'hA0};

// Data //
typedef enum logic {IN = 0, OUT = 1} direc_t;
direc_t direc_data;
logic [7:0] data_out;
assign data_io = direc_data ? data_out : 'z;

// Control Signals //
logic ce = 0;
logic oe = 0;
logic we = 0;
assign nce = ~ce;
assign noe = ~oe;
assign nwe = ~(we & clk);

// Internals //
logic [10:0] wait_cnt = '0;
logic [16:0] addr_buf = '0;

always_comb begin
    case (writer_state) 
        IDLE:    led_rgb = 'b010; // Green
        SDP:     led_rgb = 'b111; // White
        WRITE:   led_rgb = 'b110; // Orange
        WAIT:    led_rgb = 'b101; // Purple
        CHECK:   led_rgb = 'b001; // Blue
        ERROR:   led_rgb = 'b100; // Red
        default: led_rgb = 'b000; // Off
    endcase
end

always_ff @(posedge clk) begin
    if (rst) begin
        writer_state <= IDLE;
        direc_data   <= IN;
        data_out     <= '0;
        addr         <= '0;
        ce           <= 0;
        oe           <= 0;
        we           <= 0;
        sdp_cnt      <= '0;
        wait_cnt     <= '0;
        addr_buf     <= '0;
        
        if (rst_cnt == 'd15)
            int_rst <= 0;
        else
            rst_cnt <= rst_cnt + 1;
    end else begin
        case (writer_state)
            IDLE: begin
                if (btn_start) begin
                    writer_state <= SDP;
                    direc_data   <= OUT;
                    ce           <= 1;
               end
            end

            SDP: begin
                sdp_cnt    <= sdp_cnt + 1;
                addr[15:0] <= addr_sdp[sdp_cnt];
                data_out   <= data_sdp[sdp_cnt];
                we         <= 1;

                if (sdp_cnt == 'd3) begin
                    writer_state <= WRITE;
                    addr         <= addr_buf;
                    data_out     <= '0;
                end
            end

            WRITE: begin
                if (addr[6:0] == 'd127) begin
                    writer_state  <= WAIT;
                    direc_data    <= IN;
                    ce            <= 0;
                    oe            <= 0;
                    we            <= 0;
                end else begin
                    addr     <= addr + 1;
                    data_out <= data_out + 1;
                end
            end

            WAIT: begin
                wait_cnt <= wait_cnt + 1;

                if (wait_cnt == 'd1400) begin
                    writer_state <= CHECK;
                    ce           <= 1;
                    oe           <= 1;
                    wait_cnt     <= '0;
                end
            end

            CHECK: begin // Data# check
                oe <= 0;
                // Should be complement
                if (data_out[7] == data_io[7]) begin
                    writer_state <= ERROR;
                    direc_data   <= OUT;
                    ce           <= 0;
                end else begin
                    data_out <= '0;
                    addr     <= '0;

                    if (addr == 'h1FFFF) begin
                        writer_state <= IDLE;
                        direc_data   <= IN;
                        ce           <= 0;
                        addr_buf     <= '0;
                    end else begin
                        writer_state <= SDP;
                        direc_data   <= OUT;
                        addr_buf     <= addr + 1;
                    end
                end
            end

            ERROR: begin
                
            end

            default: writer_state <= IDLE;
        endcase
    end
end

endmodule
