-- |Sequence data type
-- Adapted from Data.Sequence.BSeq (sequence package)
module Data.Seq where

infixr 5 <|, :< ,|>

data Seq a = Empty
           | Leaf a
           | Node (Seq a) (Seq a)
           deriving (Show,Eq)

instance Functor Seq where
  fmap f = loop where
    loop Empty = Empty
    loop (Leaf x) = Leaf (f x)
    loop (Node l r) = Node (loop l) (loop r)

instance Foldable Seq where
  foldl f = loop where
    loop i s = case viewl s of
          EmptyL -> i
          h :< t -> loop (f i h) t
  foldr f i s = foldr f i (reverse $ toRevList s)
    where toRevList s = case viewl s of
           EmptyL -> []
           h :< t -> h : toRevList t

instance Traversable Seq where
  traverse f = loop where
    loop Empty = pure Empty
    loop (Leaf x) = Leaf <$> f x
    loop (Node l r) = Node <$> loop l <*> loop r

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: a -> Seq a -> Seq a
x <| s  =  Node (Leaf x) s

(|>) :: Seq a -> a -> Seq a
s |> x = Node s (Leaf x)

data ViewL a
  = EmptyL        -- ^ empty sequence
  | a :< Seq a    -- ^ leftmost element and the rest of the sequence

viewl Empty               = EmptyL
viewl (Leaf x)            = x :< Empty
viewl (Node (Node l r) z) = viewl (Node l (Node r z))
viewl (Node Empty r)      = viewl r
viewl (Node (Leaf x) r)   = x :< r

instance Monoid (Seq a) where
  {-# INLINE mempty #-}
  mempty = Empty
  {-# INLINE mappend #-}
  mappend = Node
  -- {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty
