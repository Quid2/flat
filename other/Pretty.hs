module Data.Flat.Pretty (
    Pretty(..),
    prettyShow,
    prettyLBS,
    prettyBS,
    prettyWord8,
    ) where
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Word
import           Text.PrettyPrint.HughesPJClass
import           Text.Printf

prettyLBS :: L.ByteString -> String
prettyLBS = render . prettyBL . L.unpack

prettyBS :: B.ByteString -> String
prettyBS = render . prettyBL . B.unpack

prettyBL :: [Word8] -> Doc
prettyBL = text . unwords . map prettyWord8

prettyWord8 :: Word8 -> String
prettyWord8 = printf "%08b"
