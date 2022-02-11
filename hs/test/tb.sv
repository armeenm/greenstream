`timescale 1ns / 1ps

module sim;

logic clk, rst, nce, noe, nwe, done, err, tx;
logic [16:0] addr;

topEntity dut
    ( .clk     (clk)
    , .rst     (rst)
    , .data_io ()
    , .rx      (tx)

    , .result_0 (nce)
    , .result_1 (noe)
    , .result_2 (nwe)
    , .result_3 (addr)
    , .result_4 (done)
    , .result_5 (err)
    );

initial begin
    clk = 0;
    rst = 0;
    tx = 1;
end

always begin
    #83 clk <= ~clk;
end

endmodule
