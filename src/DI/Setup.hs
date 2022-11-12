-- | Setup and update configs and initialization params
module DI.Setup
  ( Setup(..)
  , HasSetup
  , askSetup
  ) where

import Types

data Setup = Setup
  { toggleLogs :: IO ()
  }

type HasSetup env = HasField "setup" env Setup

askSetup :: HasSetup env => App env Setup
askSetup = asks (.setup)
