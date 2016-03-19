{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Data.Flat(module Test.Data) where
import Data.Flat
import Test.Data
import Test.Data2.Flat()

instance Flat Words
instance Flat Ints
instance Flat Void
instance Flat Unit
instance Flat N3
instance Flat N
instance Flat Un
instance Flat D2
instance Flat D4
instance Flat A
instance Flat B
instance Flat A0
instance Flat B0
instance Flat C0
instance Flat D0
instance Flat E0
instance Flat Various
instance Flat a => Flat (Phantom a)
instance Flat a => Flat (List a)
instance Flat a => Flat (ListS a)
instance Flat a => Flat (Tree a)
-- instance Flat a => Flat (Stream a)
instance   Flat a => Flat (Stream a) where
  decode = Stream <$> decode <*> decode

instance (Flat a, Flat b, Flat c) => Flat (RR a b c)
instance Flat Expr
instance Flat a => Flat (Perfect a)
instance Flat a => Flat (Fork a)

--instance (Flat a,Flat (f a),Flat (f (f a))) => Flat (PerfectF f a)
instance Flat a => Flat (Nest a)
