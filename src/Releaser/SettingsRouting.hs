{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Releaser.SettingsRouting
    ( routing
    , bnNbn
    , mapProductType
    , productTypes
    , ConfigRouting (..)
    , ConfigProcTimes (..)
    , queues
    , isRoutedOver'
    , machines
    , allBlocks
    , procTimes
    , procTimesConst
    ) where

import           Control.Arrow                       (second)
import           Data.List                           (nub, sort)
import           Data.Text                           (Text)
import           Prelude
import           SimSim                              hiding (allBlocks, productTypes,
                                                      queues)
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform
import           System.Random.MWC

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
  -- procTimesDiverging3StagesUnif
  procTimesDiverging3StagesExp

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) (configRoutingRoutes routing)


isRoutedOver' :: Block -> ProductType -> Bool
isRoutedOver' bl pt = isRoutedOver (configRoutingRoutes routing) pt bl

bnNbn :: Bool
bnNbn = True -- False

mapProductType :: ProductType -> ProductType
mapProductType x | not bnNbn = x
mapProductType (Product 1) = Product 1
mapProductType (Product 2) = Product 2
mapProductType (Product 3) = Product 1
mapProductType (Product 4) = Product 1
mapProductType (Product 5) = Product 2
mapProductType (Product 6) = Product 1
mapProductType _ = error "function mapProductType not complete in SettingsRouting"

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

test :: IO ()
test = do
  let mkTime = timeFromDouble <$> withSystemRandom (asGenIO $ genContVar (exponential (960 / 80)))
      nr = 10000
  ts <- mapM (const mkTime ) [1..nr]

  print (sum ts / fromIntegral nr)


procTimesDiverging3StagesExp :: ConfigProcTimes
procTimesDiverging3StagesExp = ConfigProcTimes "Proc Times diverging 3 stages: Exp" $
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, forAllProducts (fmap timeFromDouble . genContVar (exponential (960  / 80))))
  , (Machine 2, forAllProducts (fmap timeFromDouble . genContVar (exponential (960 / 160))))
  , (Machine 3, forAllProducts (fmap timeFromDouble . genContVar (exponential (960 / 155))))
  , (Machine 4, forAllProducts (fmap timeFromDouble . genContVar (exponential (960 / 210))))
  , (Machine 5, forAllProducts (fmap timeFromDouble . genContVar (exponential (960 / 285))))
  , (Machine 6, forAllProducts (fmap timeFromDouble . genContVar (exponential (960 / 215))))
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
