

module Releaser.Build
    ( buildBORL
    , buildSim
    , periodLength
    , productTypes
    ) where

import           Data.List                       (nub, sort)
import           System.Random                   (newStdGen)

import           Statistics.Distribution
import           Statistics.Distribution.Uniform

import           ML.BORL
import           SimSim

periodLength :: Time
periodLength = 1

buildSim :: IO SimSim
buildSim = newSimSimIO routing procTimes periodLength releaseImmediate dispatchFirstComeFirstServe shipOnDueDate

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))
                        ,(Product 2, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))])
            ,(Machine 2,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130/960) (170/960)))])
            ,(Machine 3,[(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180/960) (200/960)))])
            ]

routing :: Routes
routing =
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

productTypes :: [ProductType]
productTypes = sort $ nub $ map (fst . fst) routing

------------------------------------------------------------
--------------------------- BORL ---------------------------
------------------------------------------------------------

buildBORL :: IO (BORL s)
buildBORL = do

  undefined


