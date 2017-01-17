module Test where

-- {-# RULES "tree/one" forall n1 n2. Tag8 n1 (Tag8 n2 TagEnd) = Tag8 (n1+n2) TagEnd ; #-}

--{-# RULES "tree/oneop" forall n1 n2. tag n1 (tag n2 end) = TagEnd ; #-}

{-# RULES "tree/f" forall z. TagFiller z = z ; #-}

{-# RULES "tree/ff" forall z. (.) TagFiller z = z ; #-}

{-# RULES "tree/S" forall z. Tag8 22 (Tag8 11 z) = TagFiller z ; #-}

{-# RULES "tree/fill" fill . fill = fill; #-}

--e = Tag8 22 . Tag8 11 . TagFiller . Tag8 22 . Tag8 77 $ TagEnd


f = fill . fill . fill $ end
e = TagFiller n
n = TagFiller TagEnd
z = TagFiller $ TagFiller (TagFiller TagEnd)
zz = TagFiller . TagFiller . TagFiller $ TagEnd
g = Tag8 22 (Tag8 11 TagEnd)

--z = tag 22 (tag 33 end) -- . Tag8 11 . TagFiller . Tag8 55 . Tag8 77 $ TagEnd

{-# NOINLINE tag #-}
tag n = Tag8 n
{-# NOINLINE end #-}
end = TagEnd

{-# NOINLINE fill #-}
fill = TagFiller


data Tag = Tag8 Int Tag   | TagFiller Tag | TagEnd deriving Show
