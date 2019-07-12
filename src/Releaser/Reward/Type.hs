{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Releaser.Reward.Type
    ( ConfigReward(..)
    , configRewardOpOrds
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
configRewardOpOrds = ConfigReward 250 0.20

configRewardOrder :: ConfigReward
configRewardOrder = ConfigReward 250 1
