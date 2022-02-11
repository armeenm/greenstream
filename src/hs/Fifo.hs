module Fifo (initS, S(..), I(..), O(..), f) where

import Clash.Prelude hiding (empty)

data S = S
    { vals :: Vec 128 (BitVector 8)
    , cnt  :: Unsigned 8
    }
    deriving (Eq, Show, Generic, NFDataX)

data I = I
    { rden :: Bool
    , wren :: Bool
    , din  :: BitVector 8
    }
    deriving (Eq, Show, Generic, NFDataX)

data O = O
    { dout  :: BitVector 8
    , empty :: Bool
    , full  :: Bool
    }
    deriving (Eq, Show, Generic, NFDataX)

initS = S {vals=replicate d128 0, cnt=0}

-- Returns (empty, full) --
status S {cnt=cnt} = (cnt == 0, cnt == 128)

f :: S -> I -> (S, O)

-- Simultaneous Read/Write (Empty) --
f s@(S {cnt=0}) I {rden=True, wren=True, din=din} = (s, O {dout=din, empty=True, full=False})

-- Simultaneous Read/Write --
f s@(S {vals=vals, cnt=cnt}) I {rden=True, wren=True, din=din} = (s', o)
    where
        vals' = shiftOutFrom0 d1 $ replace cnt din vals
        s' = s {vals=fst vals'}
        o = O {dout=head $ snd vals', empty=False, full=False}

-- Read (Empty) --
f s@(S {cnt=0}) I {rden=True, wren=False} = (s, O {dout=0, empty=True, full=False})

-- Read --
f s@(S {vals=vals, cnt=cnt}) I {rden=True, wren=False} = (s', o)
    where
        vals' = shiftOutFrom0 d1 vals
        s' = s {vals=fst vals', cnt=cnt - 1}
        (empty, full) = status s'
        o = O {dout=head $ snd vals', empty=empty, full=full}

-- Write (Full) --
f s@(S {vals=vals, cnt=128}) I {wren=True, din=din} = (s', o)
    where
        vals' = shiftInAtN vals $ din :> Nil
        s' = s {vals=fst vals'}
        o = O {dout=head $ snd vals', empty=False, full=True}

-- Write --
f s@(S {vals=vals, cnt=cnt}) I {rden=False, wren=True, din=din} = (s', o)
    where
        vals' = replace cnt din vals
        s' = S {vals=vals', cnt=cnt + 1}
        (empty, _) = status s'
        o = O {dout=0, empty=empty, full=False}

f s _ = (s, O {dout=0, empty=empty, full=full})
    where
        (empty, full) = status s
