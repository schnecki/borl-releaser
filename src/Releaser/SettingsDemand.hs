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
  -- demandConst10FixedDds
  -- demandConst11FixedDds
  -- demandUniformIn3To15FixedDds
  -- demandUnif95_175
  -- demandUnif78_158
  -- demandExp105
  demandExp118
  -- demandExp135

generateOrders :: SimSim -> IO [Order]
generateOrders = configDemandFunction demand
