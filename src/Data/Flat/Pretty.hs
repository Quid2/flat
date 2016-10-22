module Data.Flat.Pretty(Pretty(..),text,prettyShow,prettyWord8) where
import           Text.PrettyPrint.HughesPJClass
import qualified Data.ByteString.Lazy           as L
import Data.Word
import           Text.Printf

instance Pretty L.ByteString where pPrint = text . prettyLBS

prettyLBS :: L.ByteString -> String
prettyLBS = unwords . map prettyWord8 . L.unpack

instance Pretty Word8 where pPrint = text . show

prettyWord8 :: Word8 -> String
prettyWord8 = printf "%08b"
