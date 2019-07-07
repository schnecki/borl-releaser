

module Releaser.Demand where


import           Statistics.Distribution.DiscreteUniform
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform

import           SimSim

interArrivalTimeDistribution :: UniformDistribution
interArrivalTimeDistribution = uniformDistr (61.935483871/960) (274.285714286/960)

productTypeDistribution :: UniformDistribution
productTypeDistribution = uniformDistr 1 2

dueDateSlack :: Time
dueDateSlack = Time 7

generateOrders :: SimSim -> IO [Order]
generateOrders sim =
  -- generateOrdersUniform sim 3 15 dueDateSlack
  generateOrdersUniform sim 9 9 dueDateSlack
  -- generateOrdersFixedDueDateSlack sim interArrivalTimeDistribution productTypeDistribution dueDateSlack
