

module Releaser.Costs where


data Costs = Costs
  { wipCosts :: Double
  , fgiCosts ::Double
  , boCosts  :: Double
  }

costConfig :: Costs
costConfig = Costs 3 10 20
