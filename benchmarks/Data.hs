{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Data where
import Flat

ggg = encode (Uno,Due,Tre)

{-
  (One Two)  Three (Four Five)
Four = 110 = 6
-}
data Numero = Uno
            | Due
            | Tre
            | Quattro
  deriving (Eq,Show,Generic,Flat)

-- chkSize 9 >> unsafeEnc 1 >>
-- chkSize 3 >> Bit8 3 val

data Booleano = Falso | Vero   deriving (Eq,Show,Generic,Flat)

data Tuple2 a b = Tuple2 a b   deriving (Eq,Show,Generic,Flat)

data Tuple3 a b c = Tuple3 a b c  deriving (Eq,Show,Generic,Flat)


