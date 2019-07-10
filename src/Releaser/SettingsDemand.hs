module Releaser.SettingsDemand
  ( demand
  , generateOrders
  , ConfigDemand (..)
  ) where

import           SimSim

import           Releaser.Demand.Ops
import           Releaser.Demand.Type

----------------------------------------

demand :: ConfigDemand
demand = demandUniformIn3To15FixedDds

generateOrders :: SimSim -> IO [Order]
generateOrders = configDemandFunction demand
