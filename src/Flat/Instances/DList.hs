module Flat.Instances.DList where

import Flat.Class
import           Flat.Instances.Mono
import Data.DList

-- $setup
-- >>> import Flat.Instances.Test
-- >>> import Flat.Instances.Base()
-- >>> import Data.DList
-- >>> let test = tstBits

{-|
>>> test (Data.DList.fromList [7::Word,7])
(True,19,"10000011 11000001 110")
-}

instance Flat a => Flat (DList a) where
  size   = sizeList . toList
  encode = encodeList . toList
  decode = fromList <$> decodeList
