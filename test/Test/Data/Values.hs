{-# LANGUAGE ExistentialQuantification #-}
module Test.Data.Values where

import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Int
import           Data.List
import qualified Data.Text            as T
import           Data.Word
import           Test.Data
import qualified Test.Data2            as D2

instance NFData a => NFData (List a)
instance NFData a => NFData (D2.List a)
instance NFData N
instance NFData a => NFData (ListS a)
instance NFData a => NFData (Stream a)
instance NFData a => NFData (Tree a)

floatT = ("float",-234.123123::Float)
doubleT = ("double",-1.91237::Double)

a01 = A0 B1 (B0 (C0 (A1 N N D2.Nil2 D2.Nil2))) (D0  E1)

ab0 = A (B (A (BB 'g')))

pe1 :: PerfectF Maybe Bool
pe1 = ConsP True (ConsP (Just False) (ConsP (Just (Just True)) NilP))

pr1 :: Pr Either List Int
pr1 = Pr (Right (C 3 N))
f1,f2,f3:: Free [] Int
f1 = Pure 1
f2 = Roll [Pure 1,Pure 2]
f3 = Roll [Roll [Pure 3],Pure 4]

rr1 :: RR Char () Int8
rr1 = RAB 'a' (RN 11 () 'b') ()

-- h = from Three
infList :: List Bool
infList = C True infList

hl1 = [1,3..111::Word]
hl2 = [1,3..111::Int]
hl3 = [False,True,True,False,True,True,True,True,False,True,True,True,True,False,True,False]

b1 = B.pack [99,173,186,44,187,124,87,186,104,99,138,202,53,137,22,5,44,244,234,7,159,119,22,234]
b2 = B.pack . concat . replicate 100 $ [235,7,135,117,255,69,100,113,113,82,128,181,200,146,155,228,144,65,83,162,130,236,235,7,135,117,255,69,100,113,113,82,128,181,200,146,155,228,144,65,83,162,130,236,235,7,135,117,255,69,100,113,113,82,128,181,200,146,155,228,144,65,83,162,130,236]

lb1 = L.pack . B.unpack $ b1
lb2 = L.pack . B.unpack $ b2

s1 = "a"
s2 = "中文版本"
s3 = ['A'..'z']
s4 = Prelude.concatMap show [1..400]

t1 = T.pack s1
t2 = T.pack s2
t3 = T.pack s3
t4 = T.pack s4

-- v1 = V1 (Just 'g') Nothing
-- v2 = V2 () (Left 45)

p1 :: Phantom Char
p1 = Phantom

--toList N = []
--toList (C h t) = h : (toList t)

l2L [] = N
l2L (x:xs) = C x (l2L xs)

l1 = l2L $ take 11 [11::Word8,22..33]

lBool :: List Bool
lBool = l2L $ map odd [1..99]

lBool2 :: List Bool
lBool2 = l2L $ map odd [1..1000]

lBool0 = C False (C True (C True (C False (C False (C False (C True (C False (C True (C False (C True (C True (C False (C False (C False N))))))))))))))

lN0 = C Three (C One N)

lN = C Three (C Three (C One (C One (C Three (C Four (C One (C Five (C Two (C Three (C Four (C Two (C Five (C Five (C Two (C Four (C Three (C One (C Four (C Five (C Two (C Five (C One (C Five (C Two (C One (C One (C Two (C Four N))))))))))))))))))))))))))))

largeSize = 1000000

lN2 :: List N
lN2 = lnx 1000

lN3 = lnx largeSize

lnx = l2L . ns

ns n = map asN [1..n]

asN = toN . (`mod` 5)

toN :: Integer -> N
toN 1 = One
toN 2 = Two
toN 3 = Three
toN 4 = Four
toN _ = Five

tree3 = mkTree largeSize

mkTree = mkTree_ 1
mkTree_ p 1 = Leaf $ asN p
mkTree_ p n = let (d,m) = n `divMod` 2
              in  Node (mkTree_ p d) (mkTree_ (p+d) (d+m))

tree1 :: Tree String
tree1 = Node (Leaf "a leaf") (Node (Leaf "and") (Leaf "more"))

tree2 :: Tree Word64
tree2 = Node (Leaf 17) (Node (Leaf 23) (Leaf 45))

-- ss = take 5 . toList $ stream1

-- stream1 = Stream True stream1


car1 = Car 2343 1965 True ModelB [18,234] "1234" [SunRoof,CruiseControl] (Engine 1200 3 9000 "Fiat" "Petrol") [Consumption 40 18,Consumption 60 23,Consumption 80 25] [(90,[Acceleration 40 12]),(110,[Acceleration 50 11])] "Fiat" "500"



treeN = Node (Leaf One) (Node (Leaf Three) (Node (Leaf Two) (Node (Node (Node (Leaf Three) (Leaf One)) (Leaf Five)) (Node (Node (Leaf Five) (Node (Node (Node (Node (Node (Leaf Two) (Leaf Four)) (Node (Leaf Three) (Node (Node (Leaf One) (Node (Leaf Four) (Node (Leaf Three) (Leaf One)))) (Node (Leaf One) (Node (Node (Leaf One) (Leaf Four)) (Leaf Two)))))) (Leaf Four)) (Node (Leaf Three) (Node (Node (Leaf Four) (Leaf Three)) (Node (Node (Leaf Five) (Node (Node (Leaf Three) (Node (Leaf Three) (Node (Node (Leaf Three) (Node (Node (Node (Node (Node (Node (Leaf Five) (Leaf Four)) (Node (Node (Node (Leaf One) (Node (Node (Leaf Four) (Node (Node (Node (Node (Node (Leaf Four) (Leaf One)) (Leaf One)) (Leaf Four)) (Leaf Five)) (Leaf Four))) (Node (Leaf Two) (Leaf Two)))) (Leaf One)) (Leaf Four))) (Leaf Two)) (Node (Leaf Five) (Node (Leaf Five) (Leaf Four)))) (Node (Leaf Three) (Leaf One))) (Node (Node (Node (Leaf Two) (Node (Node (Node (Node (Node (Leaf Five) (Leaf Three)) (Leaf Five)) (Leaf Three)) (Node (Node (Node (Leaf Two) (Node (Node (Leaf Four) (Node (Leaf One) (Node (Leaf Five) (Leaf Five)))) (Leaf Three))) (Leaf Three)) (Node (Leaf Five) (Node (Node (Node (Leaf Five) (Node (Leaf One) (Node (Node (Node (Node (Node (Node (Node (Node (Leaf Two) (Leaf Four)) (Node (Node (Node (Leaf Four) (Leaf Two)) (Node (Leaf Two) (Leaf Five))) (Leaf Two))) (Node (Node (Node (Node (Leaf Two) (Node (Node (Leaf One) (Node (Leaf Four) (Node (Node (Node (Leaf Two) (Leaf Three)) (Node (Leaf Three) (Leaf Two))) (Leaf Four)))) (Node (Node (Node (Node (Node (Node (Node (Node (Leaf Three) (Leaf Four)) (Node (Node (Leaf One) (Leaf Two)) (Leaf Four))) (Node (Node (Leaf Five) (Node (Node (Node (Leaf Two) (Leaf Five)) (Leaf Three)) (Node (Node (Node (Leaf Three) (Node (Leaf Three) (Leaf Two))) (Leaf Three)) (Leaf Three)))) (Leaf Two))) (Leaf Five)) (Node (Node (Node (Leaf One) (Node (Leaf One) (Leaf Three))) (Leaf Two)) (Leaf One))) (Node (Leaf Four) (Node (Node (Leaf Three) (Node (Leaf Five) (Leaf Five))) (Leaf Four)))) (Node (Node (Node (Leaf Five) (Leaf Four)) (Node (Leaf Five) (Node (Node (Node (Leaf One) (Leaf One)) (Leaf Four)) (Node (Node (Node (Node (Leaf Three) (Leaf Four)) (Node (Node (Leaf One) (Node (Node (Leaf Five) (Node (Node (Leaf Five) (Leaf Four)) (Leaf Three))) (Node (Node (Node (Leaf Two) (Node (Leaf Five) (Leaf Four))) (Leaf Three)) (Leaf Two)))) (Leaf Four))) (Leaf Four)) (Leaf Two))))) (Node (Leaf Two) (Node (Node (Node (Node (Node (Leaf Five) (Leaf Five)) (Leaf Four)) (Leaf Two)) (Node (Leaf One) (Node (Leaf One) (Leaf One)))) (Leaf Three))))) (Leaf Three)))) (Leaf Three)) (Node (Leaf Two) (Leaf One))) (Leaf Three))) (Node (Leaf Four) (Node (Node (Leaf Five) (Node (Leaf Two) (Leaf One))) (Node (Leaf Two) (Leaf Two))))) (Leaf Two)) (Leaf Five)) (Node (Node (Leaf One) (Leaf Five)) (Node (Leaf Four) (Leaf One)))) (Node (Leaf One) (Leaf Three))))) (Node (Node (Node (Node (Leaf One) (Leaf Two)) (Node (Node (Leaf Three) (Node (Leaf One) (Node (Node (Leaf Three) (Leaf Four)) (Leaf Three)))) (Leaf Three))) (Node (Node (Leaf Four) (Leaf One)) (Leaf Two))) (Node (Node (Node (Node (Node (Node (Node (Leaf One) (Leaf Four)) (Node (Node (Node (Leaf Four) (Leaf Four)) (Node (Node (Node (Leaf Three) (Leaf Three)) (Node (Leaf Two) (Leaf Five))) (Node (Leaf One) (Leaf Four)))) (Leaf Two))) (Leaf One)) (Leaf Four)) (Leaf Five)) (Node (Node (Node (Leaf Three) (Leaf Two)) (Node (Leaf One) (Node (Leaf Three) (Node (Leaf One) (Leaf Five))))) (Node (Node (Leaf Three) (Node (Leaf Three) (Node (Node (Leaf Five) (Leaf Three)) (Node (Node (Leaf One) (Node (Node (Leaf Three) (Node (Leaf One) (Node (Node (Leaf Two) (Leaf Two)) (Node (Leaf Three) (Node (Node (Leaf Five) (Node (Node (Leaf Four) (Node (Leaf Two) (Leaf Three))) (Node (Leaf Three) (Leaf Three)))) (Leaf Three)))))) (Leaf Four))) (Node (Leaf Three) (Leaf Five)))))) (Node (Leaf Five) (Node (Leaf Three) (Leaf One)))))) (Leaf One)))) (Leaf Four))))) (Node (Leaf One) (Leaf Two)))) (Leaf Three)) (Node (Node (Node (Leaf Five) (Leaf One)) (Node (Leaf Four) (Leaf One))) (Node (Node (Node (Leaf Three) (Node (Leaf Five) (Node (Leaf Five) (Node (Node (Leaf Four) (Leaf Four)) (Node (Node (Leaf Two) (Leaf One)) (Leaf One)))))) (Node (Node (Leaf One) (Leaf Two)) (Node (Node (Leaf Five) (Leaf Five)) (Node (Node (Node (Node (Leaf Three) (Leaf Three)) (Leaf Five)) (Node (Leaf Two) (Leaf Three))) (Leaf One))))) (Leaf One)))))) (Node (Leaf Four) (Node (Node (Leaf Two) (Node (Node (Leaf One) (Leaf Five)) (Leaf Two))) (Leaf Five)))))) (Leaf Three))) (Leaf Five))))) (Node (Leaf Four) (Leaf Four)))) (Leaf Two)))))

asciiStrT = ("asciiStr", longS $ "To hike, or not to hike? US Federal Reserve chair Janet Yellen faces a tricky decision at today's FOMC meeting. Photograph: Action Press/Rex. Theme park operator Merlin Entertainments suffered a significant drop in visitor numbers to its Alton Towers attraction after a serious rollercoaster accident in June.")

unicodeTextT = ("unicodeText",T.pack uniS)

unicodeStrT = ("unicodeStr",uniS)

uniS = longS "I promessi sposi è un celebre romanzo storico di Alessandro Manzoni, ritenuto il più famoso e il più letto tra quelli scritti in lingua italiana[1].维护和平正义 开创美好未来——习近平主席在纪念中国人民抗日战争暨世界反法西斯战争胜利70周年大会上重要讲话在国际社会引起热烈反响"

longS =  take 1000000 . concat . repeat

arr0 = ("[Bool]",map (odd . ord) $ uniS :: [Bool])

arr1 = ("[Word]",map (fromIntegral . ord) $ uniS :: [Word])

arr2 = ("ByteString from String",B.pack . map (fromIntegral . ord) $ uniS)
sbs = ("StrictByteString",b2)
lbs = ("LazyByteString",lb2)

lN2T = ("lN2",lN2)
lN3T = ("lN3",lN3)
treeNT = ("treeN",treeN)
tree3T = ("tree3",tree3)
tuple0T = ("block-tuple",(False,(),(3::Word64,33::Word,(True,(),False))))
tupleT = ("tuple",(Two,One,(Five,Three,(Three,(),Two))))
tupleBools = ("tupleBools",(False,(True,False),((True,False,True),(True,False,True))))
oneT   = ("One",One)
tupleWords = ("tupleWord",(18::Word,623723::Word,(8888::Word,823::Word)))
word8T   = ("Word8",34::Word8)
word64T   = ("Word64",34723823923::Word64)

int8T   = ("Int8",-34::Int8)
int64T   = ("Int64",-34723823923::Int64)
integerT   = ("Integer",-3472382392399239230123123::Integer)

-- Copied from binary-typed-0.3/benchmark/Criterion.hs
-- | Data with a normal form.
data NF = forall a. NFData a => NF a

-- | Evaluate 'NF' data to normal form.
force' :: NF -> ()
force' (NF x) = x `deepseq` ()

forceCafs :: IO ()
forceCafs = mapM_ (evaluate . force') cafs

-- | List of all data that should be fully evaluated before the benchmark is
--   run.
cafs :: [NF]
cafs = [NF unicodeTextT
       , NF treeN
       , NF tupleT
       , NF tuple0T
       , NF tree3T
       , NF treeNT
       , NF lN2T
       , NF lN3T
       , NF arr1
       , NF arr0
       , NF longS
       , NF uniS
       , NF unicodeStrT
       , NF unicodeTextT
       , NF asciiStrT
      ]
