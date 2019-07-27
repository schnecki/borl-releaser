
module Releaser.SettingsReward
    ( module Ops
    , rewardFunction
    ) where

import           Releaser.Reward.Ops  as Ops
import           Releaser.Reward.Type as Ops
import           Releaser.Type

rewardFunction :: RewardFunction
rewardFunction =
  RewardInFuture configRewardFutureOpOrds ByOrderPoolOrders
  -- RewardPeriodEndSimple configRewardPeriodEnd
