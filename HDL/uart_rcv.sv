module uart_rcv
    #(parameter WAIT_TIME = 0)(
    input  logic       clk,
    input  logic       rst,
    input  logic       txd_in,

    output logic [7:0] data,
    output logic       valid
);

typedef enum logic [3:0] {IDLE, START, DATA, FINISH} rcv_t;
rcv_t rcv_state;

logic       din       = 1;
logic       din_r     = 1;
logic [7:0] cycle_cnt = '0;
logic [2:0] idx       = '0;

always_ff @(posedge clk) begin
    if (rst) begin
        din_r     <= 1;
        din       <= 1;
        cycle_cnt <= '0;
        idx       <= '0;
    end else begin
        din_r <= txd_in;
        din   <= din_r;

        case (rcv_state)
            IDLE: begin
                if (din == 0)
                    rcv_state <= START;
            end

            START: begin
                if (cycle_cnt == $floor(WAIT_TIME/2)) begin
                    cycle_cnt <= '0;
                    // Ensure the bit is still low
                    if (din == 0) begin
                        rcv_state <= DATA;
                    end else
                        rcv_state <= IDLE;
                end else
                    cycle_cnt <= cycle_cnt + 1;
            end

            DATA: begin
                if (cycle_cnt == WAIT_TIME - 1) begin
                    cycle_cnt <= '0;
                    data[idx] <= din;

                    if (idx < 7)
                        idx <= idx + 1;
                    else begin
                        valid     <= 1;
                        idx       <= '0;
                        rcv_state <= FINISH;
                    end

                end else
                    cycle_cnt <= cycle_cnt + 1;
            end

            FINISH: begin
                valid     <= 0;
                rcv_state <= IDLE;
            end

            default:
                rcv_state <= IDLE;
        endcase
    end
end

endmodule
