{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Releaser.Costs.Type where

import           Data.Serialize
import           GHC.Generics

data ConfigCosts = ConfigCosts
  { wipCosts :: !Double
  , fgiCosts :: !Double
  , boCosts  :: !Double
  } deriving (Eq, Show, Generic, Serialize)
