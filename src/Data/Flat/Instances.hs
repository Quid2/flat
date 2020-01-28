
-- |Flat Instances for common data types from the packages on which `flat` has a dependency.
module Data.Flat.Instances(
    module X
) where

import           Data.Flat.Instances.Base       ( )
import           Data.Flat.Instances.ByteString ( )
import           Data.Flat.Instances.Containers ( )
import           Data.Flat.Instances.Mono       as X
import           Data.Flat.Instances.Text       as X
