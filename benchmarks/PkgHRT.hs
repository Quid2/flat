{-# LANGUAGE TemplateHaskell #-}
module Quid2Pkg (Quid2Pkg(..)) where

-- import Types
import Test.Data
import qualified Types as T

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L ( ByteString ,unpack,pack)

import Quid2.HRT.Class.Serialize as Class.Serialize
-- import Class.Serialize
import Quid2.HRT.Data.Serialize
import Quid2.HRT.Prelude

data Quid2Pkg a = Quid2Pkg a deriving (Eq,Show)

instance Serialize a => T.Serialize (Quid2Pkg a) where
  serialize (Quid2Pkg a) = L.pack . B.unpack . encode $ a
  deserialize = either (Left . error) (Right . Quid2Pkg) . decode . B.pack . L.unpack 

t = T.ser $ Quid2Pkg tree1

deriveSerialize ''Bool
deriveSerialize ''Tree
deriveSerialize ''Car
deriveSerialize ''Acceleration
deriveSerialize ''Consumption
deriveSerialize ''Model
deriveSerialize ''OptionalExtra
deriveSerialize ''Engine
