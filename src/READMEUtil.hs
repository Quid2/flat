module READMEUtil where

import Text.Printf

storeData = Stats (encTime flatData/1.5) (decTime flatData/1.5) 231
flatData = Stats (1.3) ((5.58-1.3)) 67

-- times in ms, len in bytes
data Stats = Stats {encTime::Double,decTime::Double,binSize::Int}

compareStoreFlat = table
  ["Time (mSec)","Store","Flat"]
  [["Encoding",printDouble $ encTime storeData,printDouble $ encTime flatData]
  ,["Decoding",printDouble $ decTime storeData,printDouble $ decTime flatData]
  ,transLine 1
  ,transLine 10
  ,transLine 100
  ,transferLine 1
  ,transferLine 10
  ,transferLine 100
  ]

transLine megaBytes = ["Transmission @ "++show (round megaBytes) ++" MegaByte/Sec",printDouble $ pkgTransferTime megaBytes storeData,printDouble $ pkgTransferTime megaBytes flatData]

transferLine megaBytes = ["Total Transfer @ "++show (round megaBytes) ++" MegaByte/Sec",printDouble $ pkgTotTime megaBytes storeData,printDouble $ pkgTotTime megaBytes flatData]

x = pkgTransferTime 1 storeData

pkgTotTime megaBytes pkg = encTime pkg + decTime pkg + pkgTransferTime megaBytes pkg

-- in us
pkgTransferTime megaBytes pkg = fromIntegral (binSize pkg) / (megaBytes) -- *1000)

  -- |                       | 1 MegaByte/Sec       | 10 MegaByte/Sec | 100 MegaByte/Sec |
 -- | Store Transfer Time   | ((transferTime storeData 1)) |
 -- | Flat Transfer  Time   |

printDouble :: Double -> String
printDouble = printf "%5.1f"

table header lines = Verbatim . unlines . map line $ ([header,headerLine header] ++ lines)
headerLine = map (const "---")
line headers= concatMap ('|':) headers ++ "|"

data Verbatim = Verbatim String
instance Show Verbatim where show (Verbatim s) = s
