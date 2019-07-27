{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeFamilies      #-}


module Releaser.Reward.Ops
  ( mkReward
  , SimT
  , SimTPlus1
  , configRewardFutureOpOrds
  , configRewardPosNeg1
  , configRewardPeriodEnd
  ) where

import           Control.DeepSeq
import           Data.Function          (on)
import           Data.List              (groupBy, sortBy, (\\))
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Serialize
import           GHC.Generics

import           ML.BORL
import           SimSim

import           Releaser.Reward.Type
import           Releaser.SettingsCosts
import           Releaser.Type


-- Note: the average reward is then also only in (-1,1)
configRewardPosNeg1 :: ConfigReward
configRewardPosNeg1 = ConfigReward 1 (1/277.8) (Just $ -1)

configRewardFutureOpOrds :: ConfigReward
configRewardFutureOpOrds = ConfigReward 50 1 (Just $ -50)

configRewardPeriodEnd :: ConfigReward
configRewardPeriodEnd = ConfigReward 50 0.25 (Just $ -50)


type SimT = SimSim
type SimTPlus1 = SimSim

-- | This scales the reward and ensures the result to be in the range [-baseline, baseline].
fromDouble :: ConfigReward -> Double -> Reward St
fromDouble config r = Reward $ maxFun (base - scale * r)
  where
    base = configRewardBaseline config
    scale = configRewardScale config
    maxFun = case configRewardMinimum config of
      Nothing     -> id
      Just minVal -> max minVal


mkReward :: RewardFunction -> SimT -> SimTPlus1 -> Reward St
mkReward (RewardShippedSimple config) _ sim = fromDouble config $ sum $ map (calcRewardShipped sim) (simOrdersShipped sim)
mkReward (RewardPeriodEndSimple config) _ sim = fromDouble config $ nrWipOrders * wipCosts costConfig + nrFgiOrders * fgiCosts costConfig + nrBoOrders * boCosts costConfig
  where
    currentTime = simCurrentTime sim
    nrWipOrders = fromIntegral $ M.size (simOrdersQueue sim) + M.size (simOrdersMachine sim)
    nrFgiOrders = fromIntegral $ length (simOrdersFgi sim)
    allOrdersInTheSystem = simOrdersOrderPool sim ++ concat (M.elems (simOrdersQueue sim)) ++ map fst (M.elems (simOrdersMachine sim)) ++ simOrdersFgi sim
    isBackorder order = currentTime >= dueDate order
    boOrders = filter isBackorder allOrdersInTheSystem
    nrBoOrders = fromIntegral $ length boOrders
mkReward (RewardInFuture config futureType) sim sim' = mkFutureReward config futureType sim sim'

data Future = Future
  { nrOfOrders  :: Int
  , accumulator :: Double
  , orderIds    :: [OrderId]
  } deriving (Generic, Serialize, NFData)

instance RewardFuture St where
  type StoreType St = (ConfigReward, [Future])
  applyState = applyFutureReward

instance RewardFuture StSerialisable where
  type StoreType StSerialisable = (ConfigReward, [Future])
  applyState = error "applyFutureReward should not be called for StSerialisable"


mkFutureReward :: ConfigReward -> RewardInFutureType -> SimSim -> SimSim -> Reward St
mkFutureReward config ByOrderPoolOrders sim _ = RewardFuture (config, map (\os -> Future (length os) 0 (map orderId os)) orders)
  where
    orders = groupBy ((==) `on` dueDate) $ sortBy (compare `on` dueDate) $ simOrdersOrderPool sim
mkFutureReward config ByReleasedOrders sim sim' = RewardFuture (config, [Future (length orders) 0 orders | not (null orders)])
  where
    opOrdsSim = map orderId (simOrdersOrderPool sim)
    opOrdsSim' = map orderId (simOrdersOrderPool sim')
    orders = opOrdsSim \\ opOrdsSim'


applyFutureReward :: StoreType St -> St -> Reward St
applyFutureReward (_, []) _ = RewardEmpty
applyFutureReward (config, futures) (St sim _ _ _)
  | all ((== 0) . nrOfOrders) futures = RewardEmpty
  | all (null . orderIds) futures = finalize futures
  | all null shippedOrders = RewardFuture (config, futures)
  | all (null . orderIds) futures' = finalize futures
  | otherwise = RewardFuture (config, futures')
  where
    finalize futures = avgRew $ map (\(Future _ acc _) -> fromDouble config acc) (filter ((> 0) . nrOfOrders) futures)
    avgRew xs = Reward $ sum (map rewardValue xs) / fromIntegral (max 1 (length xs))
    shippedOrders = map (\(Future _ _ orderIds) -> filter ((`elem` orderIds) . orderId) (simOrdersShipped sim)) futures
    futures' = zipWith updateFuture futures shippedOrders
    updateFuture (Future count acc openOrders) shippedOrds =
      let acc' = acc + sum (map (calcRewardShipped sim) shippedOrds)
      in Future count acc' (openOrders \\ map orderId shippedOrds)


calcRewardShipped :: SimSim -> Order -> Double
calcRewardShipped sim order =
  let fromM = timeToDouble . fromMaybe (error $ "calculating reward from order: " ++ show order)
      periodLen = timeToDouble (simPeriodLength sim)
      fullPeriodsInProd = fromIntegral $ floor (fromM (prodEnd order) / periodLen) - ceiling (fromM (released order) / periodLen)
      wip = wipCosts costConfig * fullPeriodsInProd
      periodsInFgi = fromIntegral $ floor (fromM (shipped order) / periodLen) - ceiling (fromM (prodEnd order) / periodLen)
      fgi = fgiCosts costConfig * periodsInFgi
      periodsLate = fromIntegral $ floor (fromM (shipped order) / periodLen) - ceiling (timeToDouble (dueDate order) / periodLen)
      bo = boCosts costConfig * periodsLate
   in wip + fgi + bo


