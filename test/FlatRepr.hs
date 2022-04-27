{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Data.ByteString as B
import           Data.List       (foldl')
import           Flat
import           Flat.Decoder
import           Flat.Repr
import qualified ListT           as L
import           System.TimeIt   (timeIt)

-- Big is a type that has a small encoded representation but a very large in-memory footprint.
-- It is a very large bytestring whose bytes are all set to 0
newtype Big = Big B.ByteString

newBig :: Int -> Big
newBig gigas = Big $ B.replicate (gigas*giga) 0

-- length of Big in gigas
gigas :: Big -> Int
gigas (Big b) = B.length b `div` giga

giga :: Int
giga = 1000000000

instance Show Big where show b = "Big of " ++ show (gigas b) ++ "Gbytes"

instance Flat Big where
    -- The encoded form is just the number of giga elements
    size big = size (gigas big)
    encode big = encode (gigas big)

    -- The decoded form is massive
    decode = newBig <$> decode

-- Run this as: cabal run FlatRepr -- +RTS  -M2g
main :: IO ()
main = tskip

tbig = do
    let numOfBigs = 5

    -- A serialised list of Big values
    let bigsFile = flat $ replicate numOfBigs $ newBig 1
    timeIt $ print $ B.length bigsFile

    -- tstSize bigsFile

    tstListT bigsFile

    tstRepr bigsFile

    tstBig bigsFile

-- >>> tskip
tskip = do
            let v = flat ('a',"abc",'z')
            print $ unflatWith dec v
    where
        dec :: Get (Char,Int,Char)
        dec = do
            a <- decode
            SizeOf b :: SizeOf String<- skip
            c <- decode
            return (a,b,c)

-- If we unserialise a list of Bigs and then process them (e.g. print them out) we end up in trouble, too much memory is required.
tstBig :: B.ByteString -> IO ()
tstBig bigsFile = timeIt $ do
    print "Decode to [Big]:"
    let Right (bs :: [Big]) = unflat bigsFile
    mapM_ print bs

-- So we unserialise instead them to a list of their flat representation, to be unflatted on demand later on
tstRepr :: B.ByteString -> IO ()
tstRepr bigsFile = timeIt $ do
    print "Decode to [FlatRepl Big]:"
    let Right (bsR :: [Repr Big]) = unflat bigsFile
    let bs = map unrepr bsR
    mapM_ print bs

-- Or: we extract one element at the time via a ListT
-- See http://hackage.haskell.org/package/list-t-1.0.4/docs/ListT.html
tstListT :: B.ByteString -> IO ()
tstListT bigsFile = timeIt $ do
    print "Decode to ListT IO Big:"
    stream :: L.ListT IO Big <- listTDecoder decode bigsFile
    L.traverse_ print stream


-- tstSize :: B.ByteString -> IO ()
-- tstSize bigsFile = timeIt $ do
--     print "Decode to [SizeOf Big]:"
--     let Right (bs :: [SizeOf Big]) = unflat bigsFile
--     mapM_ print bs
