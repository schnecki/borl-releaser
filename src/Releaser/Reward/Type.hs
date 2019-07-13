{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Releaser.Reward.Type
  ( ConfigReward(..)
  , configRewardOpOrds
  , configRewardOpOrdsAggressiveScaling
  , configRewardOrder
  ) where


import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics


data ConfigReward = ConfigReward
  { configRewardBaseline :: Double
  , configRewardScale    :: Double
  } deriving (Generic, Serialize, NFData, Show, Eq, Ord)


configRewardOpOrds :: ConfigReward
configRewardOpOrds = ConfigReward 250 0.5

configRewardOpOrdsAggressiveScaling :: ConfigReward
configRewardOpOrdsAggressiveScaling = ConfigReward 250 0.1


configRewardOrder :: ConfigReward
configRewardOrder = ConfigReward 250 1
