module Releaser.SettingsRouting
    ( routing
    , productTypes
    , ConfigRouting (..)
    ) where

import           Data.List             (nub, sort)
import           SimSim                hiding (productTypes)

import           Releaser.Routing.Ops
import           Releaser.Routing.Type


routing :: ConfigRouting
routing = routingDiverging2Stages

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) (configRoutingRoutes routing)
