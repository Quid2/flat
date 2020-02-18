module Flat (
    -- |Check the <https://github.com/Quid2/flat tutorial and github repo>.
    module Flat.Class,
    module Flat.Filler,
    module X, -- module Flat.Instances,
    -- module Flat.Run,
    -- UTF8Text(..),
    -- UTF16Text(..),
    Get,
    Decoded,
    DecodeException,
    ) where

import           Flat.Class
import           Flat.Decoder
import           Flat.Filler
import           Flat.Instances as X
import           Flat.Run as X
import           Flat.Types()
