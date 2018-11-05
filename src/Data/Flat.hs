module Data.Flat (
    -- |Check the <https://github.com/Quid2/flat tutorial and github repo>.
    module Data.Flat.Class,
    module Data.Flat.Decoder,
    module Data.Flat.Filler,
    module Data.Flat.Instances,
    module Data.Flat.Run,
    UTF8Text(..),
    UTF16Text(..),
    Get,
    Decoded,
    DecodeException,
    ) where

import           Data.Flat.Class
import           Data.Flat.Decoder
import           Data.Flat.Filler
import           Data.Flat.Instances
import           Data.Flat.Run
import           Data.Flat.Types
