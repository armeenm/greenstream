module uart_snd
    #(parameter WAIT_TIME = 0)(
    input  logic       clk,
    input  logic       rst,
    input  logic [7:0] data,
    input  logic       valid,

    output logic       active,
    output logic       rxd_out,
    output logic       done
);

typedef enum logic [3:0] {IDLE, START, DATA, FINISH} snd_t;
snd_t snd_state;

logic [7:0] data_r    = '0;
logic [7:0] cycle_cnt = '0;
logic [2:0] idx       = '0;

always_ff @(posedge clk) begin
    if (rst) begin
        active    <= 0;
        done      <= 0;
        snd_state <= IDLE;
        data_r    <= '0;
        cycle_cnt <= '0;
        idx       <= '0;
    end else begin
        case (snd_state)
            IDLE: begin
                rxd_out <= 1;
                done    <= 0;
                
                if (valid) begin
                    active    <= 1;
                    snd_state <= START;
                    data_r    <= data;
                end
            end

            START: begin
                rxd_out <= 0;

                if (cycle_cnt == WAIT_TIME - 1) begin
                    snd_state <= DATA;
                    cycle_cnt <= '0;
                end else
                    cycle_cnt <= cycle_cnt + 1;
            end

            DATA: begin
                rxd_out <= data_r[idx];

                if (cycle_cnt == WAIT_TIME - 1) begin
                    cycle_cnt <= '0;
                    
                    if (idx < 7)
                        idx <= idx + 1;
                    else begin
                        active    <= 0;
                        done      <= 1;
                        snd_state <= FINISH;
                        idx       <= '0;
                    end
                end else
                    cycle_cnt <= cycle_cnt + 1;
            end

            FINISH:
                snd_state <= IDLE;

            default:
                snd_state <= IDLE;
        endcase
    end
end

endmodule
