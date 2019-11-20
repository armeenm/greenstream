module uart(
    input logic        clk,
    input logic        rst,
    input logic        start_fh,
    input logic        start_rb,
    input logic        full,
    input logic        txd_in,

    output logic [7:0] data,
    output logic       wr_en,
    output logic       rxd_out
);

parameter CLK_FREQ_HZ = 7_000_000, BAUD_RATE = 115200;
localparam WAIT_TIME = $ceil(CLK_FREQ_HZ/BAUD_RATE);

typedef enum logic [2:0] {IDLE, FETCH, READBACK} uart_t;
uart_t uart_state;

byte rb_shake = 'hAA;
byte fh_shake = 'hDD;

logic handshake_q = 0;

always_ff @(posedge clk) begin
    if (rst) begin
        uart_state <= IDLE;
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
