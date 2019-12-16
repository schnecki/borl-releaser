
module Releaser.SettingsReward
    ( module Ops
    , rewardFunction
    ) where

import           Releaser.Reward.Ops  as Ops
import           Releaser.Reward.Type as Ops
import           Releaser.Type

rewardFunction :: RewardFunction
rewardFunction =
  -- RewardPeriodEndSimple configReward500
  -- RewardShippedSimple configReward500
  RewardInFuture configReward50 ByOrderPoolOrders
  -- RewardInFuture configRewardFutureOpOrds ByOrderPoolOrders
