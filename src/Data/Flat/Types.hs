{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |Common Types
module Data.Flat.Types (
    NumBits,
    module Data.Word,
    module Data.Int,
    Natural,
    SBS.ShortByteString,
    T.Text,
    UTF8Text(..),
    UTF16Text(..),
    ) where

import qualified Data.ByteString.Short.Internal as SBS
import           Data.Int
import qualified Data.Text                      as T
import           Data.Word
import           Numeric.Natural

-- ?FIX: Should be Int64 or Word64
-- |Number of bits
type NumBits = Int

-- |A wrapper to encode/decode Text as UTF8 (slower but more compact)
newtype UTF8Text = UTF8Text T.Text deriving (Eq,Ord,Show)

-- |A wrapper to encode/decode Text as UTF16 (faster but bigger)
newtype UTF16Text = UTF16Text T.Text deriving (Eq,Ord,Show)
