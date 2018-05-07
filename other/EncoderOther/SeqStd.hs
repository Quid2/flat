{-# LANGUAGE LambdaCase   #-}
-- |Sequence data type
-- Adapted from Data.Sequence.BSeq (sequence package)
module Data.SeqStd where

import           Data.Monoid hiding ((<>))
import           Data.Semigroup

infixr 5 <|, :< ,|>

data Seq a = Empty
           | Leaf a
           | Node (Seq a) (Seq a)
           deriving (Show,Eq)

type SeqZip a = ([a],[Seq a])

diffSeq :: Seq a -> ([a], [Seq a])
diffSeq = zm ([],[])
  where
    zm (as,ss) (Node l r) = zm (as,r:ss) l
    zm (as,ss) (Leaf a) = (a:as,ss)
    zm z Empty = z

-- from  bytestring-tree-builder
{-# INLINE sfoldlM #-}
sfoldlM step init =
  \case
    Empty ->
      return init
    Leaf value ->
      step init value
    Node tree1 tree2 ->
      sfoldlM step init tree1 >>= \init2 -> sfoldlM step init2 tree2

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
  deriving Show

viewl :: Seq a -> ViewL a
viewl (Node (Node l r) z) = viewl (Node l (Node r z))
viewl (Node (Leaf x) r)   = x :< r
viewl (Leaf x)            = x :< Empty
viewl Empty               = EmptyL
viewl (Node Empty r)      = viewl r

instance Semigroup (Seq a) where
  {-# INLINE (<>) #-}
  (<>) = mappend
instance Monoid (Seq a) where
  {-# INLINE mempty #-}
  mempty = Empty
  {-# INLINE [1] mappend #-}
  mappend = Node
  -- {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty
