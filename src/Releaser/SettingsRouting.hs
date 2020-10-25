{-# LANGUAGE TupleSections #-}
module Releaser.SettingsRouting
    ( routing
    , productTypes
    , ConfigRouting (..)
    , queues
    , machines
    , allBlocks
    , procTimes
    , procTimesConst
    ) where

import           Control.Arrow                   (second)
import           Data.List                       (nub, sort)
import           SimSim                          hiding (allBlocks, productTypes, queues)
import           Statistics.Distribution
import           Statistics.Distribution.Uniform

import           Releaser.Routing.Ops
import           Releaser.Routing.Type


routing :: ConfigRouting
routing =
  -- routingSingleStageSingleProduct
  -- routingSingleStage
  routingDiverging2Stages
  -- routingDiverging3Stages

procTimes :: ProcTimes
procTimes =
  procTimesDiverging2Stages
  -- procTimesDiverging3Stages

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) (configRoutingRoutes routing)


allBlocks :: [Block]
allBlocks = nub $ map (snd . fst) (configRoutingRoutes routing)

queues :: [Block]
queues = filter isQueue allBlocks


machines :: [Block]
machines = filter isMachine allBlocks


procTimesDiverging3Stages :: ProcTimes
procTimesDiverging3Stages =
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (30 / 960) (130 / 960))))
  , (Machine 2, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (80 / 960) (240 / 960))))
  , (Machine 3, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (50 / 960) (260 / 960))))
  , (Machine 4, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (50 / 960) (370 / 960))))
  , (Machine 5, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (200 / 960) (370 / 960))))
  , (Machine 6, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (110 / 960) (320 / 960))))
  ]

  where forAllProducts x = map (,x) productTypes


procTimesDiverging2Stages :: ProcTimes
procTimesDiverging2Stages =
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, [ (Product 1, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))
                , (Product 2, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))])
  , (Machine 2, [(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130 / 960) (170 / 960)))])
  , (Machine 3, [(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180 / 960) (200 / 960)))])
  ]

procTimesConst :: ProcTimes
procTimesConst =
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
    [ (Machine 1, [(Product 1, return . const (timeFromDouble (100 / 960))), (Product 2, return . const (timeFromDouble (100 / 960)))])
    , (Machine 2, [(Product 1, return . const (timeFromDouble (150 / 960)))])
    , (Machine 3, [(Product 2, return . const (timeFromDouble (190 / 960)))])
    ]
