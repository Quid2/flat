{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances ,StandaloneDeriving #-}
-- | Flat instances for the base library
module Data.Flat.Instances.Base where

import Data.Bool
import Data.Char
import Data.Fixed
import Data.Flat.Instances.Util
import Data.Complex(Complex(..))
import Data.Ratio
import Prelude hiding ( mempty )
import           Control.Monad                  ( liftM2 )
#if MIN_VERSION_base(4,9,0)
import qualified Data.List.NonEmpty as B
#endif

#if !MIN_VERSION_base(4,9,0)
deriving instance Generic (Complex a)
#endif

-- $setup
-- >>> import Data.Flat.Run(flat,unflat)
-- >>> import Data.Flat.Bits(bits)
-- >>> import Text.PrettyPrint.HughesPJClass(prettyShow)
-- >>> let tst v = (unflat (flat v) == Right v,size v 0,prettyShow . bits $ v) 

{- |
`()`, as all data types with a single constructor, has a zero-length encoding.

>>> tst ()
(True,0,"")
-}
instance Flat () where
    encode _ = mempty

    size _ = id

    decode = pure ()

{-|
One bit is plenty for a Bool.

>>> tst False
(True,1,"0")

>>> tst True
(True,1,"1")
-}
instance Flat Bool where
    encode = eBool

    size = sBool

    decode = dBool

{-|
Char's are mapped to Word32 and then encoded.

For ascii characters, the encoding is standard ascii. 

>>> tst 'a'
(True,8,"01100001")

For unicode characters, the encoding is non standard.

>>> tst 'È'
(True,16,"11001000 00000001")

>>> tst "\x1F600"
(True,26,"11000000 01110110 00000011 10")
-}
instance Flat Char where
    size = sChar

    encode = eChar

    decode = dChar

{- |
>>> tst (Nothing::Maybe Bool)
(True,1,"0")

>>> tst (Just False::Maybe Bool)
(True,2,"10")
-}
instance Flat a => Flat (Maybe a)

{-|
>>> tst (Left False::Either Bool ())
(True,2,"00")

>>> tst (Right ()::Either Bool ())
(True,1,"1")
-}
instance ( Flat a, Flat b ) => Flat (Either a b)

{-|
>>> tst (MkFixed 123 :: Fixed E0)
(True,16,"11110110 00000001")

>>> tst (MkFixed 123 :: Fixed E0) == tst (MkFixed 123 :: Fixed E2)
True
-}
instance Flat (Fixed a) where
    encode (MkFixed n) = encode n

    size (MkFixed n) = size n

    decode = MkFixed <$> decode

{- |
Word8 always take 8 bits.

>>> tst (0::Word8)
(True,8,"00000000")

>>> tst (255::Word8)
(True,8,"11111111")
-}
instance Flat Word8 where
    encode = eWord8

    decode = dWord8

    size = sWord8

{- |
Natural, Word, Word16, Word32 and Word64 are encoded as a non empty list of 7 bits chunks (least significant chunk first and most significant bit first in every chunk).

Words are always encoded in a whole number of bytes, as every chunk is 8 bits long (1 bit for the List constructor, plus 7 bits for the value).

The actual definition is:

@
Word64 ≡   Word64 Word

Word32 ≡   Word32 Word

Word16 ≡   Word16 Word

Word ≡   Word (LeastSignificantFirst (NonEmptyList (MostSignificantFirst Word7)))

LeastSignificantFirst a ≡   LeastSignificantFirst a

NonEmptyList a ≡   Elem a
                 | Cons a (NonEmptyList a)

MostSignificantFirst a ≡   MostSignificantFirst a

Word7 ≡   V0
        | V1
        | V2
        ...
        | V127
@

Values between as 0 and 127 fit in a single byte. 

127 (0b1111111) is represented as Elem V127 and encoded as: Elem=0 127=1111111

>>> tst (127::Word) 
(True,8,"01111111")

254 (0b11111110) is represented as Cons V126 (Elem V1) (254=128+126) and encoded as: Cons=1 V126=1111110 (Elem=0 V1=0000001):

>>> tst (254::Word)
(True,16,"11111110 00000001")

Another example, 32768 (Ob1000000000000000 = 0000010 0000000 0000000):

>>> tst (32768::Word32)
(True,24,"10000000 10000000 00000010")

As this is a variable length encoding, values are encoded in the same way, whatever their type:

>>> all (tst (3::Word) ==) [tst (3::Word16),tst (3::Word32),tst (3::Word64)]
True
-}
instance Flat Word where
    size = sWord

    encode = eWord

    decode = dWord

{- |
Naturals are encoded just as the fixed size Words. 

>>> tst (0::Natural)
(True,8,"00000000")

>>> tst (2^120::Natural)
(True,144,"10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000 00000010")
-}
instance Flat Natural where
    size = sNatural

    encode = eNatural

    decode = dNatural


instance Flat Word16 where
    encode = eWord16

    decode = dWord16

    size = sWord16

instance Flat Word32 where
    encode = eWord32

    decode = dWord32

    size = sWord32

instance Flat Word64 where
    encode = eWord64

    decode = dWord64

    size = sWord64


{- |
Integer, Int, Int16, Int32 and Int64 are defined as the <https://developers.google.com/protocol-buffers/docs/encoding#signed-integers ZigZag> encoded version of the equivalent unsigned Word:

@
Int   ≡  Int   (ZigZag Word)

Int64 ≡  Int64 (ZigZag Word64)

Int32 ≡  Int32 (ZigZag Word32)

Int16 ≡  Int16 (ZigZag Word16)

Int8  ≡  Int8  (ZigZag Word8)

ZigZag a ≡ ZigZag a
@

ZigZag encoding alternates between positive and negative numbers, so that numbers whose absolute value is small can be encoded efficiently:

>>> tst (0::Int)
(True,8,"00000000")

>>> tst (-1::Int)
(True,8,"00000001")

>>> tst (1::Int)
(True,8,"00000010")

>>> tst (-2::Int)
(True,8,"00000011")

>>> tst (2::Int)
(True,8,"00000100")
-}
instance Flat Int where
    size = sInt

    encode = eInt

    decode = dInt

{- |
Integers are encoded just as the fixed size Ints. 

>>> tst (0::Integer)
(True,8,"00000000")

>>> tst (-1::Integer)
(True,8,"00000001")

>>> tst (-2^120::Integer)
(True,144,"11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 00000011")
-}
instance Flat Integer where
    size = sInteger

    encode = eInteger

    decode = dInteger

{-|
>>> tst (0::Int8)
(True,8,"00000000")

>>> tst (127::Int8)
(True,8,"11111110")

>>> tst ((-128)::Int8)
(True,8,"11111111")
-}
instance Flat Int8 where
    encode = eInt8

    decode = dInt8

    size = sInt8

{- |
>>> tst (0::Int16)
(True,8,"00000000")

>>> tst (minBound::Int16)
(True,24,"11111111 11111111 00000011")

>>> tst (maxBound::Int16)
(True,24,"11111110 11111111 00000011")
-}
instance Flat Int16 where
    size = sInt16

    encode = eInt16

    decode = dInt16

{- |
>>> tst (0::Int32)
(True,8,"00000000")

>>> tst (minBound::Int32)
(True,40,"11111111 11111111 11111111 11111111 00001111")

>>> tst (maxBound::Int32)
(True,40,"11111110 11111111 11111111 11111111 00001111")
-}
instance Flat Int32 where
    size = sInt32

    encode = eInt32

    decode = dInt32

{- |
>>> tst (0::Int64)
(True,8,"00000000")

>>> tst (minBound::Int64)
(True,80,"11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 00000001")

>>> tst (maxBound::Int64)
(True,80,"11111110 11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111 00000001")
-}
instance Flat Int64 where
    size = sInt64

    encode = eInt64

    decode = dInt64


{- |
Floats are encoded as standard IEEE binary32 values:

@
IEEE_754_binary32 ≡ IEEE_754_binary32 {sign :: Sign,
                                        exponent :: MostSignificantFirst Bits8,
                                        fraction :: MostSignificantFirst Bits23}
@

>>> tst (0::Float)
(True,32,"00000000 00000000 00000000 00000000")

>>> tst (1.4012984643E-45::Float)
(True,32,"00000000 00000000 00000000 00000001")

>>> tst (1.1754942107E-38::Float)
(True,32,"00000000 01111111 11111111 11111111")
-}
instance Flat Float where
    size = sFloat

    encode = eFloat

    decode = dFloat

{- |
Doubles are encoded as standard IEEE binary64 values:

@
IEEE_754_binary64 ≡ IEEE_754_binary64 {sign :: Sign,
                                        exponent :: MostSignificantFirst Bits11,
                                        fraction :: MostSignificantFirst Bits52}
@
-}
instance Flat Double where
    size = sDouble

    encode = eDouble

    decode = dDouble

{-|
>>> tst (4 :+ 2 :: Complex Word8)
(True,16,"00000100 00000010")
-}
instance Flat a => Flat (Complex a)

{-|
Ratios are encoded as tuples of (numerator,denominator)

>>> tst (3%4::Ratio Word8)
(True,16,"00000011 00000100")
-}
instance ( Integral a, Flat a ) => Flat (Ratio a) where
    size a = size ( numerator a, denominator a )

    encode a = encode ( numerator a, denominator a )

    -- decode = uncurry (%) <$> decode
    decode = liftM2 (%) decode decode

{-|
>>> tst ([]::[Bool])
(True,1,"0")

>>> tst [False,False]
(True,5,"10100")
-}
instance {-# OVERLAPPABLE #-}Flat a => Flat [ a ]

-- Generic list instance (stack overflows with ETA, see https://github.com/typelead/eta/issues/901)
-- where
--size [] n = n+1
--size (h:t) n = trampoline size t (trampoline size h (n+1))
-- size = sizeListWith size -- foldl' (\n e -> ) n
-- encode = error "BAD"
-- encode = trampoline . encodeListWith encode
-- decode = decodeListWith decode
-- sizeListWith siz l n = foldl' (\n e -> 1 + n + siz e 0) n l
-- #ifdef ETA_VERSION
-- import Data.Function(trampoline)
-- import GHC.IO(trampolineIO)
-- #else
-- trampoline = id
-- trampolineIO = id
-- #endif

{- |
For better encoding/decoding performance, it is useful to declare instances of concrete list types, such as [Char].

>>> tst ""
(True,1,"0")

>>> tst "aaa"
(True,28,"10110000 11011000 01101100 0010")
-}
instance {-# OVERLAPPING #-}Flat [ Char ]


#if MIN_VERSION_base(4,9,0)
{-|
>>> tst (B.fromList [True])
(True,2,"10")

>>> tst (B.fromList [False,False])
(True,4,"0100")
-}
instance {-# OVERLAPPABLE #-}Flat a => Flat (B.NonEmpty a)
#endif

{- |
Tuples are supported up to 7 elements.

>>> tst (False,())
(True,1,"0")

>>> tst ((),())
(True,0,"")

"7 elements tuples ought to be enough for anybody" (Bill Gates - apocryphal)

>>> tst (False,True,True,True,False,True,True)
(True,7,"0111011")

>>> tst (1::Int,2,3,4,5,6,7,8)
...
-}

-- Not sure if these should be OVERLAPPABLE
instance {-# OVERLAPPABLE #-}( Flat a, Flat b ) => Flat ( a, b )

instance {-# OVERLAPPABLE #-}( Flat a, Flat b, Flat c ) => Flat ( a, b, c )

instance {-# OVERLAPPABLE #-}( Flat a, Flat b, Flat c, Flat d )
    => Flat ( a, b, c, d )

instance {-# OVERLAPPABLE #-}( Flat a, Flat b, Flat c, Flat d, Flat e )
    => Flat ( a, b, c, d, e )

instance {-# OVERLAPPABLE #-}( Flat a, Flat b, Flat c, Flat d, Flat e, Flat f )
    => Flat ( a, b, c, d, e, f )

instance {-# OVERLAPPABLE #-}( Flat a
                             , Flat b
                             , Flat c
                             , Flat d
                             , Flat e
                             , Flat f
                             , Flat g
                             ) => Flat ( a, b, c, d, e, f, g )


