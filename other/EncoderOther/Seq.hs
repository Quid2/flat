{-# LANGUAGE LambdaCase   #-}
-- |Sequence data type
-- Adapted from Data.Sequence.BSeq (sequence package)
module Data.Seq where

data Seq a = Empty
           | Leaf !a
           -- | a :<< Seq a
           -- | Nodes [Seq a]
           | Node (Seq a) (Seq a)
           deriving (Show,Eq)


-- data View a
--   = EmptyV        -- ^ empty sequence
--   -- | [a] ::< Seq a    -- ^ leftmost element and the rest of the sequence
--   | NodeV (View a) (View a)
--   | ListV !a (View a)    -- ^ leftmost element and the rest of the sequence
--   deriving Show

-- {-# INLINE view #-}
-- view :: View a -> Maybe (a,View a)
-- -- view (a :<< s) = a :< s
-- -- view (Node (a :<< l) r)   = a :< Node l r
-- -- view (Nodes (s:ss))   = let a :< n = view s in a :< Nodes (n ++ ss
-- view (Node (Leaf x) r)   = x :< r
-- view (Node (Node l r) z) = view (Node l (Node r z))
-- view (Leaf x)            = x :< Empty
-- view (ListV a l) = (a,l)
-- view Empty     = EmptyV

data ViewL a
  = EmptyL        -- ^ empty sequence
  -- | [a] ::< Seq a    -- ^ leftmost element and the rest of the sequence
  | !a :< Seq a    -- ^ leftmost element and the rest of the sequence
  deriving Show

{-# INLINE viewl #-}
viewl :: Seq a -> ViewL a
-- viewl (a :<< s) = a :< s
-- viewl (Node (a :<< l) r)   = a :< Node l r
-- viewl (Nodes (s:ss))   = let a :< n = viewl s in a :< Nodes (n ++ ss
viewl (Node (Leaf x) r)   = x :< r
viewl (Node (Node l r) z) = viewl (Node l (Node r z))
viewl (Leaf x)            = x :< Empty
viewl Empty               = EmptyL
viewl (Node Empty r)      = viewl r

infixr 5 <|

{-# INLINE (<|) #-}
a <| Empty = Leaf a
a <| s = Node (Leaf a) s

-- {-# INLINE viewList #-}
-- viewList :: Seq a -> [a]
-- viewList (a :<< s) = a : viewList s
-- viewList (Node (Node l r) z) = viewList (Node l (Node r z))
-- viewList (Node (Leaf x) r)   = x : viewList r
-- viewList (Node (a :<< l) r)   = a : viewList (Node l r)
-- viewList (Leaf x)            = [x]
-- viewList Empty               = []
-- viewList (Node Empty r)      = viewList r

instance Monoid (Seq a) where
  {-# INLINE mempty #-}
  mempty = Empty

  {-# INLINE [1] mappend #-}
  -- mappend (Leaf step1) seq2 = step1 :<< seq2
  -- mappend (Node (step1 :<< s1) seq) = step1 Leaf step1) seq2 = step1 :<< seq2
  --mappend l1 l2 = Node l1 l2
  mappend = Node

  -- {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty
