{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Releaser.Reward.Type
  ( ConfigReward(..)
  ) where


import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics


data ConfigReward = ConfigReward
  { configRewardBaseline :: !Double
  , configRewardScale    :: !Double
  , configRewardMinimum  :: !(Maybe Double)
  } |
  ConfigRewardCosts
  { configRewardMaximum  :: !(Maybe Double)
  }
  deriving (Generic, Serialize, NFData, Show, Eq, Ord)

