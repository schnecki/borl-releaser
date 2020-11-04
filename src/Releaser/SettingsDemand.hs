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
demand =
  -- demandConst9FixedDds
  -- demandUniformIn3To15FixedDds
  -- demandUnif95_175
  -- demandUnif78_158
  -- demandExp118
  demandExp105

generateOrders :: SimSim -> IO [Order]
generateOrders = configDemandFunction demand
