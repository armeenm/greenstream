package common;

typedef enum logic [3:0] 
    { IDLE
    , START
    , WRITE
    , READ
    , WAIT
    , CHECK
    , ERROR
    , FINISH
} st_t;

interface ctrl
    ( input  logic clk
    , input  logic rst
    , output logic active
);
    st_t st;
endinterface

interface fifo_rd 
    ( input  logic [7:0] data
    , input  logic       full
    , output logic       en
);
endinterface

interface fifo_wr
    ( output logic [7:0] data
    , input  logic       empty
    , output logic       en
);
endinterface

endpackage
