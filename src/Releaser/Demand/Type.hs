

module Releaser.Demand.Type where

import           Data.Text (Text)

import           SimSim

data ConfigDemand = ConfigDemand
  { configDemandName     :: Text
  , configDemandFunction :: SimSim -> IO [Order]
  }
