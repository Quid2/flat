-- | Flat instances for the vector package.
module Data.Flat.Instances.Vector where

import           Data.Flat.Instances.Mono
import           Data.Flat.Instances.Util

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Storable  as S

-- $setup
-- >>> import Data.Flat.Instances.Test
-- >>> import Data.Flat.Instances.Base()

{-|
Vectors are encoded as arrays.

>>> tst (V.fromList [11::Word8,22,33])
(True,40,[3,11,22,33,0])

All Vectors are encoded in the same way:

>>> let l = [11::Word8,22,33] 
>>> all (tst (V.fromList l) ==) [tst (U.fromList l),tst (S.fromList l)]
True
-}

instance Flat a => Flat (V.Vector a) where
  size = sizeSequence
  encode = encodeSequence
  decode = decodeSequence

instance (U.Unbox a,Flat a) => Flat (U.Vector a) where
  size = sizeSequence
  encode = encodeSequence
  decode = decodeSequence

instance (S.Storable a,Flat a) => Flat (S.Vector a) where
  size = sizeSequence
  encode = encodeSequence
  decode = decodeSequence
