module Eeprom (Data, Addr, M(..), initS, S(..), I(..), O(..), f) where

import Clash.Prelude
import Clash.Signal.BiSignal

type Data = BitVector 8
type Addr = Unsigned 17

-- Mode --
data M 
    = Idle 
    | Start 
    | Write 
    | Wait 
    | Check 
    | Error 
    | Finish 
    deriving (Eq, Ord, Show, Generic, NFDataX)

-- State --
data S = S
    { m     :: M
    , lastM :: M
    , cnt   :: Unsigned 11
    , addr  :: Addr
    , chk   :: Bit
    }
    deriving (Eq, Show, Generic, NFDataX)

-- Input --
data I = I
    { start      :: Bool
    , addrOffset :: Addr
    , romDin     :: Data
    , fifoDin    :: Data
    }
    deriving (Show, Generic, NFDataX)

-- Output --
data O = O
    { ce'   :: Bool
    , oe'   :: Bool
    , we'   :: Bool
    , oaddr :: Addr
    , dout  :: Maybe Data
    , rden  :: Bool
    , done  :: Bool
    , err   :: Bool
    }
    deriving (Show, Generic, NFDataX)

initS = S {m=Idle, lastM=Idle, cnt=0, addr=0, chk=low}

readO  = O {ce'=False, oe'=False, we'=True,  oaddr=0, dout=Nothing, rden=False, done=False, err=False}
writeO = O {ce'=False, oe'=True,  we'=False, oaddr=0, dout=Nothing, rden=False, done=False, err=False}

sdpAddrs :: Vec 3 (Unsigned 17) = 0x5555 :> 0x2AAA :> 0x5555 :> Nil
sdpData  :: Vec 3 (BitVector 8) = 0xAA   :> 0x55   :> 0xA0   :> Nil

f :: S -> I -> (S, O)

-- Idle --
f S {m=Idle} I {start=start}
    | start     = (initS {m=Start}, writeO)
    | otherwise = (initS, readO)

-- Start (SDP Sequence) --
f S {m=Start, cnt=cnt} _ = (s', o)
    where
        s' = if cnt < 2
            then initS {m=Wait, lastM=Start, cnt=cnt + 1}
            else initS {m=Write}

        o = writeO {oaddr=sdpAddrs !! cnt, dout=Just $ sdpData !! cnt}

-- Write --
f S {m=Write, addr=addr} I {addrOffset=addrOffset, fifoDin=din} = (s', o)
    where
        s' = if addr < 127
            then initS {m=Wait, lastM=Write, addr=addr + 1}
            else initS {m=Wait, lastM=Wait, addr=addr, chk=msb din}

        o = writeO {oaddr=addrOffset + addr, dout=Just din, rden=True}

-- Wait --
f s@(S {m=Wait, lastM=Wait, cnt=cnt, chk=chk}) _ = (s', readO)
    where
        s' = if cnt == 1400
           then initS {m=Check, chk=chk}
           else s {cnt=cnt + 1}

f s@(S {m=Wait, lastM=lastM}) _ = (s {m=lastM}, readO)

-- Check --
f s@(S {m=Check, chk=chk}) I {addrOffset=addrOffset, romDin=din} = (s', o)
    where
        s' = if msb din == chk
            then s
            else initS {m=Finish}

        o = readO {oaddr=addrOffset + 127}

-- Error --
f S {m=Error} _ = (initS, readO {err=True})

-- Finish --
f S {m=Finish} _ = (initS, readO {done=True})
