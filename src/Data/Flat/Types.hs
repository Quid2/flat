module Data.Flat.Types (
    Size,
    NumBits,
    Prim,
    S(..),
    module Data.Word,
    module Data.Int,
    Natural,
    SBS.ShortByteString,
    T.Text
    ,UTF8Text(..)
    ,UTF16Text(..)
    ) where

import qualified Data.ByteString.Short.Internal as SBS
import           Data.Int
import qualified Data.Text                      as T
import           Data.Word
import           GHC.Ptr                        (Ptr (..))
import           Numeric.Natural

type Size a = a -> NumBits -> NumBits

data S =
       S
         { nextPtr  :: {-# UNPACK #-} !(Ptr Word8)
         , currByte :: {-# UNPACK #-} !Word8
         , usedBits :: {-# UNPACK #-} !NumBits
         } deriving Show

type Prim = S -> IO S

-- FIX: Should be Int64 or Word64
type NumBits = Int

newtype UTF8Text = UTF8Text T.Text deriving (Eq,Ord,Show) -- Generic,Flat)

newtype UTF16Text = UTF16Text T.Text deriving (Eq,Ord,Show) -- Generic,Flat)
