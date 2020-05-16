{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
-- | ZigZag encoding of signed integrals (see https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba).
module Data.ZigZag
  ( ZigZag(..)
  )
where

import           Data.Word
import           Data.Int
import           Data.Bits
import           Numeric.Natural

-- $setup
-- >>> :set -XNegativeLiterals -XScopedTypeVariables -XFlexibleContexts
-- >>> import Data.Word
-- >>> import Data.Int
-- >>> import Numeric.Natural
-- >>> import Test.QuickCheck.Instances.Natural

{-|
Convert between a signed integral and the corresponding ZigZag encoded unsigned integral (e.g. between Int8 and Word8 or Integral and Natural).

Invalid conversions produce a type error:

zigZag (-1::Int64) :: Word32 
...
... Couldn't match type ...
...

>>> zigZag (0::Int8)
0

>>> zigZag (-1::Int16)
1

>>> zigZag (1::Int32)
2

>>> zigZag (-2::Int16)
3

>>> zigZag (-50::Integer)
99

>>> zigZag (50::Integer)
100

>>> zigZag (64::Integer)
128

>>> zigZag (-256::Integer)
511

>>> zigZag (256::Integer)
512

>>> map zigZag [-3..3::Integer]
[5,3,1,0,2,4,6]

>>> map zagZig [0..6::Word8]
[0,-1,1,-2,2,-3,3]

prop> \(f::Integer) -> zagZig (zigZag f) == f

prop> \(f::Natural) -> zigZag (zagZig f) == f

prop> \(f::Int8) -> zagZig (zigZag f) == f
prop> \(f::Word8) -> zigZag (zagZig f) == f
prop> \(s::Int8) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer))
prop> \(u::Word8) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural))

prop> \(f::Int64) -> zagZig (zigZag f) == f
prop> \(f::Word64) -> zigZag (zagZig f) == f
prop> \(s::Int64) -> zigZag s == fromIntegral (zigZag (fromIntegral s :: Integer))
prop> \(u::Word64) -> zagZig u == fromIntegral (zagZig (fromIntegral u :: Natural))
-}

-- Allow conversion only between compatible types
class (Integral signed,Integral unsigned) => ZigZag signed unsigned | unsigned -> signed,signed -> unsigned where
  zigZag :: signed -> unsigned
  default zigZag :: FiniteBits signed => signed -> unsigned
  zigZag s = fromIntegral ((s `shiftL` 1) `xor` (s `shiftR` (finiteBitSize s - 1)))
  {-# INLINE zigZag #-}

  zagZig :: unsigned -> signed
  default zagZig :: (Bits unsigned) => unsigned -> signed
  zagZig u = fromIntegral ((u `shiftR` 1) `xor` (negate (u .&. 1)))

  -- default zagZig :: (Bits signed) => unsigned -> signed
  -- zagZig u = let (s::signed) = fromIntegral u in ((s `shiftR` 1) `xor` (negate (s .&. 1)))
  {-# INLINE zagZig #-}

instance ZigZag Int8 Word8
instance ZigZag Int16 Word16
instance ZigZag Int32 Word32
instance ZigZag Int64 Word64
instance ZigZag Integer Natural where
  zigZag x | x >= 0    = fromIntegral $ x `shiftL` 1
           | otherwise = fromIntegral $ negate (x `shiftL` 1) - 1
  zagZig u =
    let s = fromIntegral u in ((s `shiftR` 1) `xor` (negate (s .&. 1)))
