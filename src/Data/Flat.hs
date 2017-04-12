module Data.Flat (
    -- |Check the <https://github.com/tittoassini/flat tutorial and github repo>.
    module X,
    UTF8Text(..),
    UTF16Text(..),
    Get,
    Decoded,
    DecodeException,
    ) where

import           Data.Flat.Class     as X
import           Data.Flat.Decoder
import           Data.Flat.Filler    as X
import           Data.Flat.Instances as X
import           Data.Flat.Run       as X
import           Data.Flat.Types
