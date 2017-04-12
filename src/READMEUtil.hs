{-# LANGUAGE CPP #-}
module READMEUtil where

import Text.Printf
import qualified Data.Text as T

storeData = Stats 3.1 22.6 702728
flatData = Stats 7 30 114841
-- storeData = Stats (encTime flatData/1.5) (decTime flatData/1.5) 231
-- flatData = Stats (1.3) ((5.58-1.3)) 67

-- times in ms, len in bytes
data Stats = Stats {encTime::Double,decTime::Double,binSize::Int}

compareStoreFlat = T.pack $ table
  ["","Store","Flat"]
  [["Encoding (mSec)",printDouble $ encTime storeData,printDouble $ encTime flatData]
  ,["Decoding (mSec)",printDouble $ decTime storeData,printDouble $ decTime flatData]
  ,["Size (bytes)",show $ binSize storeData,show $ binSize flatData]
  ,transLine 1
  ,transLine 10
  ,transLine 100
  ,transferLine 1
  ,transferLine 10
  ,transferLine 100
  ]

transLine megaBytes = ["Transmission (mSec) @ "++show (round megaBytes) ++" MegaByte/Sec",printDouble $ pkgTransferTime megaBytes storeData,printDouble $ pkgTransferTime megaBytes flatData]

transferLine megaBytes = ["Total Transfer (mSec) @ "++show (round megaBytes) ++" MegaByte/Sec",printDouble $ pkgTotTime megaBytes storeData,printDouble $ pkgTotTime megaBytes flatData]

x = pkgTransferTime 1 storeData

pkgTotTime megaBytes pkg = encTime pkg + decTime pkg + pkgTransferTime megaBytes pkg

-- in ms
pkgTransferTime megaBytes pkg = fromIntegral (binSize pkg) / (megaBytes *1000)

table header lines = unlines . map line $ ([header,headerLine header] ++ lines)
headerLine = map (const "---")
line headers= concatMap ('|':) headers ++ "|"

-- Add Verbatim support
-- NOT WORKING
-- #define EXTENSIONS "instance {-# OVERLAPPING #-} IO_ Verbatim where io_  = return . showText"
-- data Verbatim = Verbatim String
-- instance Show Verbatim where show (Verbatim s) = s

printDouble :: Double -> String
printDouble = printf "%5.1f"


-- printInt :: Int -> String
-- printInt = splitAt 3 . reverse . show 
