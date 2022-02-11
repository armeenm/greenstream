module Uart (M(..), initS, S(..), I(..), O(..), f) where

{-# LANGUAGE RecordWildCards #-}

import Clash.Prelude

{-
clkFreq  = 7000000 :: Int -- Hz
baudRate = 9600    :: Int -- bps
waitTime = ceiling $ clkFreq `div` baudRate
-}

waitCyc = 730
halfCyc = 365

-- Mode --
data M
    = Idle 
    | Start 
    | Data 
    | Stop
    | Finish 
    deriving (Eq, Ord, Show, Generic, NFDataX)

-- State --
data S = S
    { m      :: M
    , cycCnt :: Unsigned 10
    , bitCnt :: Unsigned 3
    , dat    :: Vec 8 Bit
    , rxrr   :: Bit
    , rxr    :: Bit
    }
    deriving (Eq, Show, Generic, NFDataX)

-- Input --
type I = Bit

-- Output --
data O = O
    { dout :: BitVector 8
    , vld  :: Bool
    , actv :: Bool
    }
    deriving (Eq, Show, Generic, NFDataX)

updateRegs rxrr rxr rx = (rxr, rx)

initS = S {m=Idle, cycCnt=0, bitCnt=0, dat=replicate d8 0, rxrr=high, rxr=high}

defaultO = O {dout=0, vld=False, actv=False}
activeO  = O {dout=0, vld=False, actv=True}

f :: S -> I -> (S, O)

-- Idle --
f S {m=Idle, rxrr=rxrr, rxr=rxr} rx
    | rxrr == low = (initS {m=Start, rxrr=rxrr', rxr=rxr'}, defaultO)
    where
        (rxrr', rxr') = updateRegs rxrr rxr rx

-- Start --
f s@(S {m=Start, cycCnt=cycCnt, rxrr=rxrr, rxr=rxr}) rx
    | cycCnt == halfCyc =
        if rxrr == low -- Make sure it's still low
        then (initS {m=Data, rxrr=rxrr', rxr=rxr'}, activeO)
        else (initS {rxrr=rxrr', rxr=rxr'}, defaultO)
    | otherwise = (s {cycCnt=cycCnt + 1, rxrr=rxrr', rxr=rxr'}, activeO)
    where
        (rxrr', rxr') = updateRegs rxrr rxr rx

-- Data --
f s@(S {m=Data, ..}) rx
    | cycCnt == waitCyc = (initS {m=nextMode, bitCnt=bitCnt + 1, dat=dat', rxrr=rxrr', rxr=rxr'}, out)
    | otherwise         = (s {cycCnt=cycCnt + 1, rxrr=rxrr', rxr=rxr'}, out)
    where
        dat' = fst $ shiftInAtN dat $ rx :> Nil
        nextMode = if bitCnt == 7 then Stop else Data
        out = activeO
        (rxrr', rxr') = updateRegs rxrr rxr rx

-- Stop --
f s@(S {m=Stop, ..}) rx
    | cycCnt == waitCyc = (s {m=Finish, cycCnt=0, rxrr=rxrr', rxr=rxr'}, activeO {dout=v2bv dat, vld=True})
    | otherwise         = (s {cycCnt=cycCnt + 1, rxrr=rxrr', rxr=rxr'}, activeO)
    where
        (rxrr', rxr') = updateRegs rxrr rxr rx

-- Finish --
f S {m=Finish, dat=dat} rx = (initS, defaultO {dout=v2bv dat, vld=True})
    where
        (rxrr', rxr') = updateRegs rxrr rxr rx

-- Default --
f S {..} rx = (initS {rxrr=rxrr', rxr=rxr'}, defaultO) 
    where
        (rxrr', rxr') = updateRegs rxrr rxr rx
