module uart(
    input logic        clk,
    input logic        rst,
    input logic        start_fh,
    input logic        start_rb,
    input logic        txd_in,
    input logic        full,
    input logic        empty,
    input logic  [7:0] din,

    output logic [7:0] dout,
    output logic       wr_en,
    output logic       rd_en,
    output logic       rxd_out
);

parameter  CLK_FREQ_HZ = 7_000_000, BAUD_RATE = 115200;
localparam WAIT_TIME   = $ceil(CLK_FREQ_HZ/BAUD_RATE);

typedef enum logic [2:0] {IDLE, FETCH, READBACK} uart_t;
uart_t uart_state;

byte rb_shake = 'hAA;
byte fh_shake = 'hDD;

uart_rcv #(.WAIT_TIME(WAIT_TIME)) rcv(
    .clk    (clk),
    .rst    (rst),
    .txd_in (txd_in),

    .data   (dout),
    .valid  (wr_en)
);

logic snd_start, snd_active, snd_done;
uart_snd #(.WAIT_TIME(WAIT_TIME)) snd(
    .clk     (clk),
    .rst     (rst),
    .rxd_out (rxd_out),
    .data    (din),
    .valid   (snd_start),

    .active  (snd_active),
    .done    (snd_done)
);

logic handshake_q = 0;

always_ff @(posedge clk) begin
    if (rst) begin
        uart_state  <= IDLE;
        snd_start   <= 0;
        handshake_q <= 0;
    end else begin
        case (uart_state)
            IDLE: begin
               if (start_rb)
                   uart_state <= READBACK;
               else if (start_fh)
                   uart_state <= FETCH;
            end

            READBACK: begin
                if (!handshake_q) begin
                    
                end
            end

            default: begin
                uart_state <= IDLE;
            end
        endcase
    end
end

endmodule
