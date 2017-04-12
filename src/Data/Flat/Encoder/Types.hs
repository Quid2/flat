-- |Encoder Types
module Data.Flat.Encoder.Types(
  Size,
  NumBits,
  Prim,
  S(..)
) where

import           Data.Flat.Types
import           GHC.Ptr         (Ptr (..))

-- |Calculate the size (in bits) of the encoding of a value
type Size a = a -> NumBits -> NumBits

-- |Strict encoder state
data S =
       S
         { nextPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , currByte :: {-# UNPACK #-} !Word8
         , usedBits :: {-# UNPACK #-} !NumBits
         } deriving Show

-- |A basic encoder
type Prim = S -> IO S

