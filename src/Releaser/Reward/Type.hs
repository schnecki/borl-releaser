{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Releaser.Reward.Type
  ( ConfigReward(..)
  , configRewardFutureOpOrds
  , configRewardPosNeg1
  , configRewardPeriodEnd
  ) where


import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics


data ConfigReward = ConfigReward
  { configRewardBaseline :: Double
  , configRewardScale    :: Double
  , configRewardMinimum  :: Maybe Double
  } deriving (Generic, Serialize, NFData, Show, Eq, Ord)

-- Note: the average reward is then also only in (-1,1)
configRewardPosNeg1 :: ConfigReward
configRewardPosNeg1 = ConfigReward 1 (1/277.8) (Just $ -1)

configRewardFutureOpOrds :: ConfigReward
configRewardFutureOpOrds = ConfigReward 250 0.3 (Just $ -250)

configRewardPeriodEnd :: ConfigReward
configRewardPeriodEnd = ConfigReward 200 1.0 (Just $ -200)
