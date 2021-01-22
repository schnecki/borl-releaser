{-# LANGUAGE OverloadedStrings #-}

module Releaser.Routing.Ops
    ( routingDiverging2Stages
    , routingSingleStage
    , routingSingleStageSingleProduct
    , routingDiverging3Stages
    ) where

import           SimSim

import           Releaser.Routing.Type

routingSingleStageSingleProduct :: ConfigRouting
routingSingleStageSingleProduct = ConfigRouting "Single stage, single product"
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> FGI
  ]


routingSingleStage :: ConfigRouting
routingSingleStage = ConfigRouting "Diverging with 2 stages and no return visits"
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> FGI

  , (Product 2, OrderPool) --> Queue 1   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 2, Machine 1) --> FGI
  ]

routingDiverging2Stages :: ConfigRouting
routingDiverging2Stages = ConfigRouting "Diverging with 2 stages and no return visits"
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 1   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 2, Machine 1) --> Queue 3
  , (Product 2, Queue 3)   --> Machine 3
  , (Product 2, Machine 3) --> FGI
  ]


-- isRoutedOver :: Routes -> ProductType -> Block -> Bool
-- isRoutedOver routes pt bl = bl `elem` blocks
--   where
--     routing = filter ((== pt) . fst . fst) routes
--     blocks = map snd routes ++ map (snd . fst) routes

-- 1: M1 -> M2 -> M4
-- 2: M1 -> M2 -> M5
-- 3: M1 -> M2 -> M6
-- 4: M1 -> M3 -> M4
-- 5: M1 -> M3 -> M5
-- 6: M1 -> M3 -> M6
routingDiverging3Stages :: ConfigRouting
routingDiverging3Stages = ConfigRouting "Diverging with 3 stages and no return visits"
  [
  -- P1
    (Product 1, OrderPool) --> Queue 1
  , (Product 1, Queue 1)   --> Machine 1
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> Queue 4
  , (Product 1, Queue 4)   --> Machine 4
  , (Product 1, Machine 4) --> FGI

  -- P2
  , (Product 2, OrderPool) --> Queue 1
  , (Product 2, Queue 1)   --> Machine 1
  , (Product 2, Machine 1) --> Queue 2
  , (Product 2, Queue 2)   --> Machine 2
  , (Product 2, Machine 2) --> Queue 5
  , (Product 2, Queue 5)   --> Machine 5
  , (Product 2, Machine 5) --> FGI


  -- P3
  , (Product 3, OrderPool) --> Queue 1
  , (Product 3, Queue 1)   --> Machine 1
  , (Product 3, Machine 1) --> Queue 2
  , (Product 3, Queue 2)   --> Machine 2
  , (Product 3, Machine 2) --> Queue 6
  , (Product 3, Queue 6)   --> Machine 6
  , (Product 3, Machine 6) --> FGI


  , (Product 4, OrderPool) --> Queue 1
  , (Product 4, Queue 1)   --> Machine 1
  , (Product 4, Machine 1) --> Queue 3
  , (Product 4, Queue 3)   --> Machine 3
  , (Product 4, Machine 3) --> Queue 4
  , (Product 4, Queue 4)   --> Machine 4
  , (Product 4, Machine 4) --> FGI

  , (Product 5, OrderPool) --> Queue 1
  , (Product 5, Queue 1)   --> Machine 1
  , (Product 5, Machine 1) --> Queue 3
  , (Product 5, Queue 3)   --> Machine 3
  , (Product 5, Machine 3) --> Queue 5
  , (Product 5, Queue 5)   --> Machine 5
  , (Product 5, Machine 5) --> FGI

  , (Product 6, OrderPool) --> Queue 1
  , (Product 6, Queue 1)   --> Machine 1
  , (Product 6, Machine 1) --> Queue 3
  , (Product 6, Queue 3)   --> Machine 3
  , (Product 6, Machine 3) --> Queue 6
  , (Product 6, Queue 6)   --> Machine 6
  , (Product 6, Machine 6) --> FGI

  ]
