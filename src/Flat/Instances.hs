
-- |Flat Instances for common data types from the packages on which `flat` has a dependency.
module Flat.Instances(
    module X
) where

import           Flat.Instances.Base       ( )
import           Flat.Instances.ByteString ( )
import           Flat.Instances.Containers ( )
import           Flat.Instances.Mono       as X
import           Flat.Instances.Text       as X
