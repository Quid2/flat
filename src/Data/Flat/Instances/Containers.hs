{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving ,DeriveGeneric #-}

{-# LANGUAGE CPP #-}
-- |Instances for the containers library
module Data.Flat.Instances.Containers (sizeMap
  , encodeMap
  , decodeMap
) where

import Data.Flat.Instances.Util
import Data.Map 
import Data.Flat.Instances.Base()
import Data.Flat.Instances.Mono
import Data.Tree                                                
import Data.Set
import Data.IntMap 
import Data.Sequence

-- $setup
-- >>> import Data.Flat.Instances.Test

{-|
Maps are defined as a list of (Key,Value) tuples:

@
Map = List (Key,Value)

List a = Nil | Cons a (List a)
@
-}

{-|
>>> tst $ Data.IntMap.empty
(True,1,[0])

>>> asList Data.IntMap.fromList [(1,"a"),(2,"b")]
True
-}
instance Flat a => Flat (IntMap a) where
  size = sizeMap
  encode = encodeMap
  decode = decodeMap

{-|
Maps are encoded as lists:

>>> tst $ Data.Map.empty
(True,1,[0])

>>>  asList Data.Map.fromList [(1,"a"),(2,"b")]
True

Key/Values are encoded in order:

>>> let l = [(2,"b"),(1,"a")] in tst (Data.Map.fromList l) == tst (Data.Map.fromList $ Prelude.reverse l)
True

IntMap and Map are encoded in the same way:

>>> let l = [(2,"b"),(1,"a")] in tst (Data.IntMap.fromList l) == tst (Data.Map.fromList l)
True
-}
instance (Flat a, Flat b, Ord a) => Flat (Map a b) where
  size = sizeMap
  encode = encodeMap
  decode = decodeMap

{-|
Data.Sequence.Seq is encoded as a list

>>> asList Data.Sequence.fromList [3::Word8,4,7]
True
-}
instance Flat a => Flat (Seq a) where
 size = sizeList -- . toList
 encode = encodeList -- . Data.Sequence.toList
 decode = Data.Sequence.fromList <$> decodeList

{-|
Data.Set is encoded as a list

>>> asList Data.Set.fromList [3::Word8,4,7]
True
-}
instance (Flat a,Ord a) => Flat (Set a) where
  size = sizeSet
  encode = encodeSet
  decode = decodeSet

{-|
Data.Tree

>>>  tst (Node (1::Word8) [Node 2 [Node 3 []], Node 4 []])
(True,39,[1,129,64,200,32])
-}
#if ! MIN_VERSION_containers(0,5,8)
deriving instance Generic (Tree a)
#endif

instance (Flat a) => Flat (Tree a)