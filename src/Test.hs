{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase    ,ScopedTypeVariables    #-}
module Test where
import           Data.Binary.FloatCast
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Coerce
import           Data.Flat
import           Data.Flat.Pretty
import           Data.Int
import qualified Data.Sequence                  as S
import qualified Data.Text                      as T
import           Data.Word
import           Prelude                        hiding (exponent, sign)
import           System.Endian
import           Text.Printf

rrr = reverse [3,2,1]

e = getSystemEndianness

data Logico = Falso | Vero deriving (Generic,Show,Flat)

data LLL a = NIL | CONS a (LLL a) deriving (Generic,Show)

data NN = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | N11
  deriving (Generic,Show,Flat)

data ZZZ = ZZ1 | ZZ2 | ZZ3 | ZZ4 | ZZ5
  deriving (Generic,Show,Flat)

instance {-# OVERLAPPABLE #-} Flat a => Flat (LLL a)

instance {-# OVERLAPPING #-} Flat (NN,NN,NN)

instance {-# OVERLAPPING #-} Flat (LLL NN)

newtype A = A Bool
newtype B = B Bool
data C = C Bool

kkk = [[127,1],[128,1,1],[129,1,1],[255,127,1],[128,128,1,1],[129,128,1,1],[255,255,1,1],[128,128,2,1],[129,128,2,1],[255,255,127,1],[128,128,128,1,1],[129,128,128,1,1]] == map (L.unpack . flat) [127::Word32,128,129,16383,16384,16385,32767,32768,32769,2097151,2097152,2097153]

-- qqq = size N4 0 + size N5 0

--k :: Decoded String
k = --let v = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let v = [0::Int16,0,0,0,0,0,0,0,0,0,64,64,0,0,64,64,64,64,64,64,0]
  in (unflat . flat $ v) == Right v

-- instance Flat NN where
--   encode = \case
--     Uno -> eBits 2 0
--     Due ->  eBits 2 1
--     Tre -> eBits 2 2
--     Quattro -> eBits 3 6
--     Cinque -> eBits 3 7

data Uno = Uno deriving (Generic,Show,Flat)

data TT = TT1 | TT2  deriving (Generic,Show,Flat)

data FF = FF Float Double deriving (Generic,Show,Flat)

data WWW = WWW Word8 Word16 Word32 Word64 Bool deriving (Generic,Show,Flat)

data XYZ = W2 Word8 Word8
         | W3 Bool Bool deriving (Generic,Show,Flat)

data T2 a b = T2 a b deriving (Generic,Show)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic,Show) --,Flat)

data ABC = AA
         | BB
         | CC
         | DD Bool Bool
         -- | ZZ ABC
         | WW Word8 Word16 Word32 Word64 Bool
         | FFF Float Double Float Double
         | GGG
  deriving (Generic, Show, Flat)

-- instance Flat a => Flat (Tree a)
instance {-# OVERLAPPABLE #-} (Flat a,Flat b) => Flat (T2 a b)

instance {-# OVERLAPPING #-} Flat (T2 Bool Bool)

-- insta-ddump-opt-cmmnce (Flat a,Flat b) => Flat (T2 a b) where
--  encode (T2 a b) = chkWriter (encode a) [encode b] <> encode a

instance {-# OVERLAPPABLE #-} Flat a => Flat (Tree a)

instance {-# OVERLAPPING #-} Flat (Tree ABC)

instance {-# OVERLAPPING #-} Flat (Tree NN)

-- instance Flat a => Flat (Tree a) where
--  encode (Leaf a) = chkWriters [eFalse,encode a]
--  encode (Node a b) = chkWriters [eTrue,encode a,encode b]
-- 125 [240::Word8,88,90,33,22,96,176,180,66,44,240,88,90,33,22,96,176,180,66,44,193,97,104,132,89]
xx = px n2

-- [133,197,164,33]
xy = px [11,22,33::Word8]

-- [193,97,104,132,89]
n1 :: Tree Word8
n1 = Node (Node (Leaf 11) (Leaf 22)) (Node (Leaf 33) (Leaf 44))

-- [240,88,90,33,22,96,176,180,66,44,224,176,180,66,44,193,97,104,132,89]
n2 = Node (Node n1 n1) (Node (Node n1 n1) n1)

u =  pp $ WW 11 22 33 44 True
uu =  pp (11::Word32,22::Word64,33::Word64)
--o = encodings $ postAligned $ T2 (11::Word32) (22::Word64)
ww = unflat (flat (0::Word16)) :: Decoded Word16
--encodings a = gencoders (from a) []

t = do
  pp $ (Vero,Falso)
  pp $ S.fromList "aaa"
  pp ""
  pp "aaa"
  pp (Uno,'a',Uno,'a',Uno,'a')
  pp (T.pack $ take 200 $ repeat 't')
  pp (take 200 $ repeat 's')
  pp (T.pack $ "aaa")
  pp Uno
  pp (-22::Int)
  pp (18446744073709551615::Word)
  pp (18446744073709551615::Word64)
  pp (2*34723823940::Word64)
  pp (-34723823923::Int64)
  pp (22::Int8)
  pp (22::Int16)
  pp (N1,N1,N1)
  pp N1
  pp N2
  pp N3
  pp N4
  pp N5
  pp AA
  pp ZZ2
  pp $ W2 4 4
  pp $ W3 False True
  pp $ DD True False
  pp $ (True,True,False,True) -- 11010001
  --pp $ ZZ AA
  pp $ WW 0 0 0 0 False
  pp $ WWW 0 0 0 0 False
  pp $ FF 0.0 1.2
  pp 'a'
  pp (11::Word64)
  pp False
  pp (False,True)
  pp (False,AA,True)
  pp [False,False]
  pp [AA,BB]
  pp "aaa"
  pp (Node (Node (Leaf True) (Leaf False)) (Node (Leaf True) (Leaf False)))
  pf (Node (Node (Leaf True) (Leaf False)) (Node (Leaf True) (Leaf False)))

--pp v = print (v,F.encode v F.<> F.eFiller)
pp :: forall a . (Flat a, Show a) => a -> IO ()
-- pp v = putStrLn (unwords [show v,"->",show (size :: Size a),show $ getSize (postAligned v),show $ encode v,"->",show $ L.unpack $ flat v])
-- pp v = putStrLn (unwords [show v,"->",show (size :: Size a),show $ getSize v,show $ encode v,"->",show $ L.unpack $ flat v])
pp v = putStrLn (unwords [show v,"->",show $ getSize v,show $ encode v,"->",show $ L.unpack $ flat v])

-- gg :: Flat a => a -> Vector Bool
gg = valueBits . flat $ "abc"

shBS = SBS.toShort stBS
lzBS = L.pack bs
stBS = B.pack bs
bs = [32,32,32::Word8]

-- px v = putStrLn (unwords [show v,"->",show $ encode v,"->",show $ L.unpack $ flatRaw (T2 v FillerEnd)])

px v = let bs = L.unpack $ flat v in putStrLn (unwords [show v,"->",show $ encode v,"->",show bs,show $ sum bs])

pf v = putStrLn (unwords [show v,"->",show $ flat v])

tc = coerce (B True) :: A

f1 = wordToDouble 0x3ff0000000000000 -- ::Word64

ll = flat [True,False,True]

f3 = doubleToWord 1

f2 =flat (1::Double)

kk = flat [(55::Word64,18::Word16),(5599::Word64,1122::Word16)]
-- tt = encode [(55::Word64,18::Word16,True,AA)]
e1 = encode (True,False) -- (AA,BB)
e2 = encode [AA,BB]
e3 = encode [True,False]
e4 = encode $ DD True True
-- e5 = encode $ ZZ (ZZ (ZZ (DD True True)))

{-# NOINLINE tup2 #-}
tup2 a b = flat (a,b)
--t = coerce (1::Word32) :: Float
-- b = L.unpack $ flat $ B.pack $ replicate 250 33
b = L.unpack $ flat $ (11::Word8,SBS.toShort $ B.replicate 400 33) -- [1..255] B.pack bs

w = L.unpack $ flat (2::Word16,2::Word32,2::Word64)

l = L.unpack $ flat [1::Word16,1,1]

j = L.unpack $ flat "abc"

js = L.unpack $ flat (S.fromList "abc")

jj = L.unpack $ flat $ (11::Word8,T.pack "\x1F600\&000aaa维维aaa")

jl = T.length (T.pack "\x1F600\&\x1F600\&")

q :: Decoded T.Text
q = unflat $ flat (T.pack "D\226\FStz\GS3]\n8\149sV\243J\181\181\235\214&y\226\231\&2\239\212\174\DC1J'F\129hpsu\199\178")

g = let v = (False,1::Word8,0::Word8,False)
    in (unflat (flat v) == Right v,valueBits v)

g1 :: Decoded (Bool,Bool,Bool,Bool)
g1 = unflat $ flat (True,False,True,True)

g2 :: Decoded (Bool,Word8,Word8,Bool)
g2 = unflat $ flat (False,1::Word8,1::Word8,False)

g3 :: Decoded Word8
g3 = unflat $ flat (0::Word8)

g4 :: Decoded (Bool,Word8,Bool)
g4 = unflat $ flat (False,0::Word8,False)

g5 :: Decoded (Float,Double,Bool,Float,Double,Bool)
g5 = unflat $ flat (8.11E-11::Float,8.11E-11::Double,True,8.11E-11::Float,8.11E-11::Double,True)

f0 = valueBits ((False,255::Word8,False,255::Word8))
f = valueBits (False,0::Word8,False)

p :: String
p = printf "%032b" $ floatToWord (-0.15625)

x = prettyLBS $ flatRaw (-0.15625::Double)

dd = prettyLBS $ flatRaw (-123.2325E-23::Double)

d = prettyLBS $ flatRaw (3/0::Double)

s = serRaw (True,False,True)

z = valueBits $ (True,False,True)
zz = L.unpack . flat $ (True,False,True)

serRaw :: Flat a => a -> [Word8]
--serRaw = L.unpack . flatRaw
serRaw = asBytes . valueBits

y :: Decoded Float
y = unflat $ flat (-0.15625::Double)

-- i :: Decoded IEEE_754_binary32
-- i = unflat $ flat (-0.15625::Float)

--t = i == Right (IEEE_754_binary32 {sign = V1, exponent = Bits8 V0 V1 V1 V1 V1 V1 V0 V0, fraction = (Bits7 V0 V1 V0 V0 V0 V0 V0,Bits8 V0 V0 V0 V0 V0 V0 V0 V0,Bits8 V0 V0 V0 V0 V0 V0 V0 V0)})
bb = 0b111

c = (isInfinite (3/0),isNegativeZero (-0::Double),isNaN (0/0::Float),(0/0::Float)==(0/0::Float))

