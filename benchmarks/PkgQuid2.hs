{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses ,DeriveGeneric ,DeriveDataTypeable ,ScopedTypeVariables ,GADTs ,NoMonomorphismRestriction ,DeriveGeneric ,DefaultSignatures ,TemplateHaskell ,TypeFamilies ,FlexibleContexts ,PackageImports ,FlexibleInstances ,CPP #-}
module PkgQuid2(PkgQuid2(..)
               ,Binary
               -- ,QQ.serialize
               --,deserializeOrFail,deserializePartial
               ,DeserializeFailure(..),serlN2
               ) where

import Data.Word

import Control.Exception
import Data.Monoid
import Control.Applicative
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Builder as BS
import Data.Typeable
-- import Criterion.Main
-- import Data.Binary.Serialize.ByteBuilder
-- import qualified Data.Binary.Serialize.ByteBuilder as QQ

import Data.Binary.Quid2
import qualified Data.Binary.Quid2 as QQ
import Data.Binary.Encoding(eBits)

import qualified Data.ByteString.Lazy                           as L
import Types
import Test.Data hiding (ss)
import L

-- x = let Encoding e = encode (33::Int32) in e OutStreamEnd

t =  ((ss1 == [128+64+16,64],ss1 == [1,1,0,1,0,0,0,0,0,1])
     ,(ss2 == bitTreelN,ss2==bytelN))

-- z :: Either DeserializeFailure (List N)
z = let Right l2 = QQ.deserialize . QQ.serialize $ lN
    in l2 == lN

{- h :: Either String (List N)
h = QQ.deserializeBit 66 -- (192)
-}

bb = q (C True (C False N))

qq = q Four

x = (encode Two,encode Five)

g = (q (Two,Two),q One,q Two,q Three,q Four,q Five)

q = BS.unpack . QQ.serialize

-- y :: Either QQ.DeserializeFailure Word16
-- y = dd . ss $ (257::Word16)

-- t2 n = tst (take n $ repeat 'a')

tst v = let Right v2 = dd $ ss v
        in v == v2

dd = QQ.deserialize . L.pack

ss = L.unpack . QQ.serialize

u = (\v -> let Right (PkgQuid2 a) = Types.deserialize PkgQuid2.serlN2 in a == v) lN2

-- x = (s (33::Word64),s (200::Word))

s = ser . PkgQuid2

z2 :: Either DeserializeFailure (List N)
z2 = QQ.deserialize $ BS.pack bitTreelN

-- PROB: Something is always returned
e :: Either DeserializeFailure (List N)
e = QQ.deserialize $ BS.pack bitFixlN

l2 = (length bitTreelN,length bitFixlN,length bytelN)

bitTreelN = [72,4,193,202,98,238,89,6,114,225,200,5,160]
bitFixlN  = [34,0,35,4,18,49,68,19,32,52,20,4,16,1,56]
bytelN = [0,2,0,2,0,0,0,0,0,2,0,3,0,0,0,4,0,1,0,2,0,3,0,1,0,4,0,4,0,1,0,3,0,2,0,0,0,3,0,4,0,1,0,4,0,0,0,4,0,1,0,0,0,0,0,1,0,3,1]

ss1 = (BS.unpack $ QQ.serialize $ (True,True,False,True,False,False,False,False,False,True))
ss2 = BS.unpack $ QQ.serialize $ lN
ss3 = BS.unpack $ QQ.serialize $ lN2

serlN2 = BS.pack [5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,5,51,130,153,193,76,224,166,112,83,56,41,156,20,206,10,103,128]

data PkgQuid2 a = PkgQuid2 {unPkg::a} deriving (Eq,Show)

instance Arbitrary a => Arbitrary (PkgQuid2 a) where arbitrary = fmap PkgQuid2 arbitrary

{-
instance Binary a => Serialize (PkgQuid2 a) where
  serialize (PkgQuid2 a) = QQ.serialize a
  deserialize = either (Left . toException) (Right . PkgQuid2) . QQ.deserialize
-}

-- instance (Decodable a,Binary a) => Serialize PkgQuid2 a where
instance (Binary a) => Serialize PkgQuid2 a where
  serialize (PkgQuid2 a) = QQ.serialize a
  deserialize = either (Left . toException) (Right . PkgQuid2) . QQ.deserialize
  pkg = PkgQuid2
  unpkg (PkgQuid2 a) = a

-- data B = F | T deriving Generic
-- instance Binary B

---------- Instances

-- instance Decodable Bool where decoder = Nxt (Pure False) (Pure True)

{-
instance Decodable N where decoder = Nxt (Nxt (Pure One) (Pure Two)) (Nxt (Pure Three) (Nxt (Pure Four) (Pure Five)))

instance Decodable a => Decodable (List a) where decoder = Nxt (C <$> decoder <*> decoder) (Pure N)

instance Decodable a => Decodable (Tree a) where decoder = Nxt (Node <$> decoder <*> decoder) (Leaf <$> decoder)
-}

{- hand coded
instance Binary N where
{-
   encode One = {-# SCC encode_N #-} tag 3 0
    encode Two = {-# SCC encode_N #-} tag 3 1
    encode Three = {-# SCC encode_N #-} tag 3 2
    encode Four = {-# SCC encode_N #-} tag 3 3
    encode Five = {-# SCC encode_N #-} tag 3 4
-}

    encode One = {-# SCC encode_N #-} tag 2 0
    encode Two = {-# SCC encode_N #-} tag 2 1
    encode Three = {-# SCC encode_N #-} tag 2 2
    encode Four = {-# SCC encode_N #-} tag 3 6
    encode Five = {-# SCC encode_N #-} tag 3 7

{-
      |             |
    |    |      |        |
   One  Two   Three   |     |
                     Four Five
-}
    --bitDecoder = Nxt (Nxt (Done One) (Done Two)) (Nxt (Done Three) (Nxt (Done Four) (Done Five)))

    -- hand optimised

    get = {-# SCC "get.N" #-} do

      tag <- getBit

      case tag of
        False -> do
          tag <- getBit
          case tag of
            False -> return One
            True -> return Two
        True -> do
          tag <- getBit
          case tag of
            False -> return Three
            True -> do
                 tag <- getBit
                 case tag of
                   False -> return Four
                   True -> return Five

{- Faster?
      tag <- getBits 2

      case tag of
        0 -> return One
        1 -> return Two
        2 -> return Three
        _ -> do
          tag <- getBit -- s 1
          case tag of
            0 -> return Four
            1 -> return Five
-}
{-
bd :: Binary a => BitDecoder (Maybe a)
bd = Nxt (Just `fmap` bitDecoder) (Done Nothing)
-}

instance Binary a => Binary (List a) where
  encode (C v l) = {-# SCC encode_List_C #-} tag 1 0 <> encode v <> encode l
  encode N = {-# SCC encode_List_N #-} tag 1 1

  -- bitDecoder = Nxt ((C `fmap` bitDecoder) `fmap` bitDecoder) (Done N)
  -- bitDecoder = Nxt (Done $ C bitDecoder bitDecoder) (Done N)

  get = {-# SCC "get.List" #-} do
      tag <- getBit -- s True

      case tag of
        False -> C <$> get <*> get
        True -> return N

instance Binary a => Binary (Tree a) where
  encode (Node tTrue t2) = {-# SCC encode_Tree_Node #-} tag 1 0 <> encode tTrue <> encode t2
  encode (Leaf a) = {-# SCC encode_Tree_Leaf #-} tag 1 1 <> encode a

  get = {-# SCC "get.Tree" #-} do
      tag <- getBit -- s True

      case tag of
        False -> Node <$> get <*> get
        True -> Leaf <$> get
-}

{-
-- Super specialised, save another 10%
instance Binary (List N) where
  encode (C v l) = {-# SCC encode_ListN_C #-} tag 1 0 <> encode v <> encode l
  encode N = {-# SCC encode_ListN_N #-} tag 1 1

instance Binary (List Bool) where
  encode (C v l) = {-# SCC encode_ListBool_C #-} tag 1 0 <> encode v <> encode l
  encode N = {-# SCC encode_ListBool_N #-} tag 1 1
-}

#define INSTANCES_GENERICS

{- Generics based -}
#ifdef INSTANCES_GENERICS
instance Binary a => Binary (List a)
-- instance Binary (List Bool)
-- instance Binary (List N)
instance Binary N
-- instance Binary Bool
instance Binary Ordering
instance Binary a => Binary (L a)
instance Binary a => Binary (Tree a)
#endif

{-
instance Binary Car
instance Binary Acceleration
instance Binary Consumption
instance Binary Model
instance Binary OptionalExtra
instance Binary Engine
-}


instance Binary Un
