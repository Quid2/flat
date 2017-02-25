{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts ,FlexibleInstances ,LambdaCase #-}
module Test.Data.Flat(module Test.Data) where
import Data.Flat
import Data.Flat.Encoder
import Test.Data
import Test.Data2.Flat()
import Data.Word
import Data.Flat.Pokes
import Data.Foldable
import Data.Int

-- GHC 8.0.2 chokes on this
-- instance Flat A0
-- instance Flat B0
-- instance Flat C0
-- instance Flat D0
-- instance Flat E0

instance Flat a => Flat (List a)
instance Flat Unit

instance (Flat a, Flat b, Flat c) => Flat (RR a b c)

instance Flat Direction
instance Flat Words
instance Flat Ints
instance Flat Void

instance Flat N3
instance Flat Un

instance Flat a => Flat (ListS a)

instance Flat D2
instance Flat D4
instance Flat A
instance Flat B
instance Flat Various


instance Flat a => Flat (Phantom a)
instance Flat Expr
instance Flat a => Flat (Perfect a)
instance Flat a => Flat (Fork a)

--instance (Flat a,Flat (f a),Flat (f (f a))) => Flat (PerfectF f a)
instance Flat a => Flat (Nest a)

-- instance Flat a => Flat (Stream a)
instance   Flat a => Flat (Stream a) where
  decode = Stream <$> decode <*> decode

{-
              |
    |
One Two               |
                Three     |
                      Four Five
 -}
-- instance {-# OVERLAPPABLE #-} Flat a => Flat (Tree a) where
--   encode (Node t1 t2) = eFalse <> encode t1 <> encode t2
--   encode (Leaf a) = eTrue <> encode a

-- instance {-# OVERLAPPING #-} Flat (Tree N) where
--   encode (Node t1 t2) = eFalse <> encode t1 <> encode t2
--   encode (Leaf a) = eTrue <> encode a

-- -- -34% (why?)
-- instance Flat N where
--   {-# INLINE encode #-}
--   encode = \case
--     One -> eBits 2 0
--     Two -> eBits 2 1
--     Three -> eBits 2 2
--     Four -> eBits 3 6
--     Five -> eBits 3 7

instance Flat N -- where
  -- {-# INLINE size #-}
  -- size n s = s + case n of
  --   One -> 2 
  --   Two -> 2
  --   Three -> 2
  --   Four -> 3
  --   Five -> 3

instance {-# OVERLAPPING #-} Flat (Tree N) -- where
-- --   {-# INLINE encode #-}
--   encode (Node t1 t2) = Writer $ \s -> do
--     !s1 <- runWriter eFalse s
--     !s2 <- runWriter (encode t1) s1
--     s3 <- runWriter (encode t2) s2
--     return s3

  -- encode (Leaf a) = Writer $ \s -> do
  --   s1 <- runWriter eTrue s
  --   runWriter (encode a) s1

--   size (Node t1 t2) = 1 + size t1 + size t2
--   size (Leaf a) = 1 + size a

--instance Flat N
instance {-# OVERLAPPABLE #-} Flat a => Flat (Tree a)

-- instance {-# OVERLAPPING #-} Flat (Tree N)
instance {-# OVERLAPPING #-} Flat (Tree (N,N,N)) --where
--   size (Node t1 t2) = 1 + size t1 + size t2
--   size (Leaf a) = 1 + size a

-- -57%
instance {-# OVERLAPPING #-} Flat [N] -- where size = foldl' (\s n -> s + 1 + size n) 1

instance {-# OVERLAPPING #-} Flat (N,N,N) -- where
  -- {-# INLINE size #-}
  -- size (n1,n2,n3) = size n1 + size n2 + size n3

-- -50%
-- instance {-# OVERLAPPING #-} Flat (N,N,N) where
--    {-# INLINE encode #-}
--    encode (n1,n2,n3) = wprim $ (Step 9) (encodeN n1 >=> encodeN n2 >=> encodeN n3)
-- {-# INLINE encodeN #-}
-- encodeN = \case
--     One -> eBitsF 2 0
--     Two ->  eBitsF 2 1
--     Three -> eBitsF 2 2
--     Four -> eBitsF 3 6
--     Five -> eBitsF 3 7

-- -20%
instance {-# OVERLAPPING #-} Flat (Word,Word8,Word16,Word32,Word64) -- where size (n1,n2,n3,n4,n5) = size n1 + size n2 + size n3 + size n4 + size n5



