{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Releaser.SettingsRouting
    ( routing
    , productTypes
    , ConfigRouting (..)
    , ConfigProcTimes (..)
    , queues
    , machines
    , allBlocks
    , procTimes
    , procTimesConst
    ) where

import           Control.Arrow                       (second)
import           Data.List                           (nub, sort)
import           Data.Text                           (Text)
import           SimSim                              hiding (allBlocks, productTypes,
                                                      queues)
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform

import           Releaser.Routing.Ops
import           Releaser.Routing.Type


routing :: ConfigRouting
routing =
  -- routingSingleStageSingleProduct
  -- routingSingleStage
  -- routingDiverging2Stages
  routingDiverging3Stages

procTimes :: ConfigProcTimes
procTimes =
  -- procTimesDiverging2Stages
  procTimesDiverging3StagesUnif
  -- procTimesDiverging3StagesExp

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) (configRoutingRoutes routing)


allBlocks :: [Block]
allBlocks = nub $ map (snd . fst) (configRoutingRoutes routing)

queues :: [Block]
queues = filter isQueue allBlocks


machines :: [Block]
machines = filter isMachine allBlocks


--

data ConfigProcTimes = ConfigProcTimes
  { configProcTimesName :: !Text
  , configProcTimes     :: !ProcTimes
  }


procTimesDiverging3StagesExp :: ConfigProcTimes
procTimesDiverging3StagesExp = ConfigProcTimes "Proc Times diverging 3 stages: Exp" $
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, forAllProducts (fmap timeFromDouble . genContVar (exponential (80  / 960))))
  , (Machine 2, forAllProducts (fmap timeFromDouble . genContVar (exponential (160 / 960))))
  , (Machine 3, forAllProducts (fmap timeFromDouble . genContVar (exponential (155 / 960))))
  , (Machine 4, forAllProducts (fmap timeFromDouble . genContVar (exponential (210 / 960))))
  , (Machine 5, forAllProducts (fmap timeFromDouble . genContVar (exponential (285 / 960))))
  , (Machine 6, forAllProducts (fmap timeFromDouble . genContVar (exponential (215 / 960))))
  ]

  where forAllProducts x = map (,x) productTypes

procTimesDiverging3StagesUnif :: ConfigProcTimes
procTimesDiverging3StagesUnif = ConfigProcTimes "Proc Times diverging 3 stages: Unif" $
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (30 / 960) (130 / 960))))
  , (Machine 2, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (80 / 960) (240 / 960))))
  , (Machine 3, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (50 / 960) (260 / 960))))
  , (Machine 4, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (50 / 960) (370 / 960))))
  , (Machine 5, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (200 / 960) (370 / 960))))
  , (Machine 6, forAllProducts (fmap timeFromDouble . genContVar (uniformDistr (110 / 960) (320 / 960))))
  ]

  where forAllProducts x = map (,x) productTypes


procTimesDiverging2Stages :: ConfigProcTimes
procTimesDiverging2Stages = ConfigProcTimes "Proc Times diverging 2 stages: Unif" $
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, [ (Product 1, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))
                , (Product 2, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))])
  , (Machine 2, [(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130 / 960) (170 / 960)))])
  , (Machine 3, [(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180 / 960) (200 / 960)))])
  ]

procTimesConst :: ConfigProcTimes
procTimesConst = ConfigProcTimes "Proc Times diverging 2 stages: Const" $
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
    [ (Machine 1, [(Product 1, return . const (timeFromDouble (100 / 960))), (Product 2, return . const (timeFromDouble (100 / 960)))])
    , (Machine 2, [(Product 1, return . const (timeFromDouble (150 / 960)))])
    , (Machine 3, [(Product 2, return . const (timeFromDouble (190 / 960)))])
    ]
