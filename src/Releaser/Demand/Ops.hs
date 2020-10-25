{-# LANGUAGE OverloadedStrings #-}

module Releaser.Demand.Ops
  ( demandUniformIn3To15FixedDds
  , demandConst9FixedDds
  , demandUnif95_175
  ) where


import           Statistics.Distribution.DiscreteUniform
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform

import           SimSim

import           Releaser.Demand.Type
import           Releaser.SettingsPeriod
import           Releaser.Util

dds :: Integer
dds = 7

dueDateSlack :: Time
dueDateSlack = fromInteger dds * periodLength


demandUniformIn3To15FixedDds :: ConfigDemand
demandUniformIn3To15FixedDds = ConfigDemand ("U(3,15) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 3 15 dueDateSlack)

demandConst9FixedDds :: ConfigDemand
demandConst9FixedDds = ConfigDemand ("Const(9) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 9 9 dueDateSlack)

demandUnif95_175 :: ConfigDemand
demandUnif95_175 =
  ConfigDemand
    ("Uniform interarrival-time w/ Unif(95,175) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
        in generateOrdersFixedDueDateSlack sim (uniformDistr (95 / 960) (175 / 960)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)


-- interArrivalTimeDistribution :: UniformDistribution
-- interArrivalTimeDistribution = uniformDistr (61.935483871/960) (274.285714286/960)

-- productTypeDistribution :: UniformDistribution
-- productTypeDistribution = uniformDistr 1 2

-- demandExponential

  -- generateOrdersFixedDueDateSlack sim interArrivalTimeDistribution productTypeDistribution dueDateSlack
