{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Flat 
import Flat.Decoder.Types ( Get )
import Data.List (foldl')
import qualified Data.ByteString as B

-- Big is a type that has a small encoded representation but a very large in-memory footprint.
-- It is a very large bytestring whose bytes are all set to 0
newtype Big = Big B.ByteString

newBig :: Int -> Big
newBig giga = Big $ B.replicate (giga*1000000000) 0

-- length of Big in gigas
gigas :: Big -> Int
gigas (Big b) = B.length b `div` 1000000000

instance Show Big where show b = "Big of " ++ show (gigas b) ++ "Gbytes"

instance Flat Big where
    -- The encoded form is just the number of giga elements
    size big = size (gigas big)
    encode big = encode (gigas big)

    -- The decoded form is massive
    decode = newBig <$> decode

-- Run this as: cabal run FlatRepr -- +RTS  -M2g
main :: IO ()
main = do
    let numOfBigs = 5

    -- A serialised list of Big values
    let bigsFile = flat $ replicate numOfBigs $ newBig 1

    tstRepr bigsFile

    tstBig bigsFile 

-- If we unserialise a list of Bigs and then process them (e.g. print them out) we end up in trouble, too much memory.
tstBig :: B.ByteString -> IO ()
tstBig bigsFile = do
    print "Decode to [Big]:"
    let Right (bs :: [Big]) = unflat bigsFile
    mapM_ print bs

-- So we unserialise instead them to a list of their flat representation, to be unflatted on demand later on
tstRepr :: B.ByteString -> IO ()
tstRepr bigsFile = do
    print "Decode to [FlatRepl Big]:"
    let Right (bsR :: [FlatRepr Big]) = unflat bigsFile
    let bs = map unrepr bsR
    mapM_ print bs

-- Flat representation of a value
newtype FlatRepr a = FlatRepr {repr :: B.ByteString}

-- Get the underlying value
unrepr :: Flat a => FlatRepr a -> a
unrepr (FlatRepr bs)= let Right a = unflat bs in a

instance Flat a => Flat (FlatRepr a) where
    size  = error "unused"
    encode = error "unused"

    -- To create the representation we just re 'flat' the parsed value (this could be optimised by copying directly the parsed representation)
    decode = FlatRepr . flat <$> (decode :: Get a)

