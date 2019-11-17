`timescale 1ns / 1ps

module sim;

logic clk, nce, noe, nwe, rst, start;
logic [16:0] addr;
typedef enum logic {IN = 0, OUT = 1} direc_t;
direc_t direc_data;
logic [7:0] data_io;
assign data_io = direc_data ? '0 : 'z;

top dut(
    .data_io (),
    .addr (addr),
    .nce (nce),
    .noe (noe),
    .nwe (nwe),

    .sysclk (clk),
    .btn_rst (rst),
    .btn_start (start)
);

initial begin
    clk        = 0;
    rst        = 0;
    start      = 1;
    #1000000 start = 0;
end

always begin
    #83 clk <= ~clk;
    assert(noe == 1 || data_io !== 'z) else begin
        $fatal("You will burn things!");
    end
end

endmodule
