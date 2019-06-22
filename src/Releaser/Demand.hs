

module Releaser.Demand where


import           ClassyPrelude                           (tshow)
import           Control.DeepSeq
import           Control.Monad.Trans.Reader
import           Data.Foldable                           (toList)
import           Data.List                               (nub)
import qualified Data.Map.Strict                         as M
import           Data.Maybe                              (fromMaybe)
import           Statistics.Distribution
import           Statistics.Distribution.DiscreteUniform
import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform

import           ML.BORL
import           SimSim

import           Releaser.Type


interArrivalTimeDistribution :: UniformDistribution
interArrivalTimeDistribution = uniformDistr (64/960) (320/960)

productTypeDistribution :: UniformDistribution
productTypeDistribution = uniformDistr 1 2

dueDateSlack :: Time
dueDateSlack = Time 7

generateOrders :: SimSim -> IO [Order]
generateOrders sim = generateOrdersFixedDueDateSlack sim interArrivalTimeDistribution productTypeDistribution 7
