module Releaser.SettingsRouting
    ( routing
    , productTypes
    , ConfigRouting (..)
    , queues
    , machines
    , allBlocks
    ) where

import           Data.List             (nub, sort)
import           SimSim                hiding (allBlocks, productTypes, queues)

import           Releaser.Routing.Ops
import           Releaser.Routing.Type


routing :: ConfigRouting
routing =
  routingSingleStage
  -- routingDiverging2Stages

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) (configRoutingRoutes routing)


allBlocks :: [Block]
allBlocks = map (snd . fst) (configRoutingRoutes routing)

queues :: [Block]
queues = filter isQueue allBlocks


machines :: [Block]
machines = filter isMachine allBlocks

