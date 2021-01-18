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
  -- demandUnif78_158 -- 80%
  demandUnif95_175  -- 70%
  -- demandExp105  -- 90%
  -- demandExp118  -- 80%
  -- demandExp135  -- 70%

generateOrders :: SimSim -> IO [Order]
generateOrders = configDemandFunction demand
