{-# LANGUAGE FlexibleInstances #-}
module Flat.Instances.Extra where

import           Flat.Instances.Base ()
import Flat.Class ( Flat )

-- $setup
-- >>> import Flat.Instances.Test

{- |
For better encoding/decoding performance, it is useful to declare instances of concrete list types, such as [Char].

>>> tstBits ""
(True,1,"0")

>>> tstBits "aaa"
(True,28,"10110000 11011000 01101100 0010")
-}
instance {-# OVERLAPPING #-} Flat [Char]

