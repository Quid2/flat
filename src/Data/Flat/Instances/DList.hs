module Data.Flat.Instances.DList where

import Data.Flat.Class
import           Data.Flat.Instances.Mono
import Data.DList

-- $setup
-- >>> import Data.Flat.Instances.Test
-- >>> import Data.Flat.Instances.Base()
-- >>> let test = tstBits

{-|
>>> test (Data.DList.fromList [7::Word,7])
(True,19,"10000011 11000001 110")
-}

instance Flat a => Flat (DList a) where
  size   = sizeList . toList
  encode = encodeList . toList
  decode = fromList <$> decodeList
