{-# LANGUAGE OverloadedStrings #-}

module Releaser.Routing.Ops
    ( routingDiverging2Stages
    , routingSingleStage
    , routingSingleStageSingleProduct
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


