module Greenstream where

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Clash.Prelude hiding (Floating)
import Clash.Signal.BiSignal
import Clash.Explicit.Testbench
import qualified Eeprom
import qualified Fifo
import qualified Uart

data S = S
    { offset  :: Eeprom.Addr
    , uartS   :: Uart.S
    , fifoS   :: Fifo.S
    , eepromS :: Eeprom.S
    }
    deriving (Eq, Show, Generic, NFDataX)

data I = I
    { rx   :: Bit
    , tdin :: Eeprom.Data
    }
    deriving (Eq, Show, Generic, NFDataX)

data O = O 
    { ce'  :: Bool
    , oe'  :: Bool
    , we'  :: Bool
    , addr :: Eeprom.Addr
    , dout :: Maybe Eeprom.Data
    , done :: Bool
    , err  :: Bool
    }
    deriving (Eq, Show, Generic, NFDataX)

initS = S {offset=0, uartS=Uart.initS, fifoS=Fifo.initS, eepromS=Eeprom.initS}

f :: S -> I -> (S, O)

f S {..} I {..} = (s', o)
    where
        (uartS', Uart.O {Uart.dout=udout, Uart.vld=uvld, Uart.actv=uactv})
            = Uart.f uartS rx

        (fifoS', Fifo.O {Fifo.dout=fdout, Fifo.empty=fempty, Fifo.full=ffull})
            = Fifo.f fifoS Fifo.I {Fifo.rden=erden, Fifo.wren=uvld, Fifo.din=udout}

        (eepromS', Eeprom.O 
            { Eeprom.ce'=ce'
            , Eeprom.oe'=oe'
            , Eeprom.we'=we'
            , Eeprom.oaddr=eaddr
            , Eeprom.dout=edout
            , Eeprom.rden=erden
            , Eeprom.done=edone
            , Eeprom.err=eerr
            }) = Eeprom.f eepromS Eeprom.I 
            { Eeprom.start=ffull
            , Eeprom.addrOffset=offset
            , Eeprom.romDin=tdin
            , Eeprom.fifoDin=fdout
            }

        offset'
            = if edone
            then offset + 128
            else offset

        s' = S {eepromS=eepromS', fifoS=fifoS', uartS=uartS', offset=offset'}
        o  = O {dout=edout, addr=eaddr, done=True, err=eerr, ..}

f' s (tdin, rx) = (s', (dout, o))
    where
        (s', O {..}) = f s I {..}
        o = (ce', oe', we', addr, done, err)

{-# ANN topEntity
    (Synthesize
        { t_name   = "topEntity"
        , t_inputs = [ PortName "clk"
                     , PortName "rst"
                     , PortProduct ""
                         [ PortName "data_io"
                         , PortName "rx"
                         ]
                     ]
        , t_output = PortProduct ""
                        [ PortName "data2"
                        , PortProduct ""
                            [ PortName "nce"
                            , PortName "noe"
                            , PortName "nwe"
                            , PortName "addr"
                            , PortName "done"
                            , PortName "err"
                            ]
                        ]
        }) #-}
topEntity
    :: Clock System
    -> Reset System
    -> (BiSignalIn Floating System (BitSize Eeprom.Data), Signal System Bit)
    -> 
    ( BiSignalOut Floating System (BitSize Eeprom.Data)
    , Signal System (Bool, Bool, Bool, Eeprom.Addr, Bool, Bool)
    )

topEntity clk rst (dat, rx) = (writeToBiSignal dat dout, o)
    where
        i = bundle (readFromBiSignal dat, rx)
        (dout, o) = (exposeClockResetEnable $ unbundle $ mealy f' initS i) clk rst enableGen
{-# NOINLINE topEntity #-}

{-
testBench :: Signal System Bool
testBench = done
    where
        testInput      = stimuliGenerator clk rst $(listToVecTH [(1, 1) :: (Signed 9, Signed 9), (2, 2), (3, 3)])
        expectedTopOut = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9, 1, 5])
        done           = expectedTopOut (topEntity clk rst (testInput))
        clk            = tbSystemClockGen (not <$> done)
        rst            = systemResetGen
        -}
