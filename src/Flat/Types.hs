{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |Common Types
module Flat.Types (
    NumBits,
    module Data.Word,
    module Data.Int,
    Natural,
    SBS.ShortByteString,
    T.Text,
    -- UTF8Text(..),
    -- UTF16Text(..),
    ) where

import qualified Data.ByteString.Short.Internal as SBS
import           Data.Int
import qualified Data.Text                      as T
import           Data.Word
import           Numeric.Natural

-- |Number of bits
type NumBits = Int

