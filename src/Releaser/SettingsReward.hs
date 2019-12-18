
module Releaser.SettingsReward
    ( module Ops
    , rewardFunction
    ) where

import           Releaser.Reward.Ops  as Ops
import           Releaser.Reward.Type as Ops
import           Releaser.Type

rewardFunction :: RewardFunction
rewardFunction =
  RewardPeriodEndSimple configReward500

  -- RewardShippedSimple configReward500
  -- RewardInFuture configReward500 ByOrderPoolOrders
  -- RewardInFuture configRewardFutureOpOrds ByOrderPoolOrders
  -- RewardInFuture configReward500 ByReleasedOrders