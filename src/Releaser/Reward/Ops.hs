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
  ) where

import           Data.List              ((\\))
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe)
import           Data.Serialize
import           GHC.Generics

import           ML.BORL
import           SimSim

import           Releaser.Reward.Type
import           Releaser.SettingsCosts
import           Releaser.Type


type SimT = SimSim
type SimTPlus1 = SimSim


fromDouble :: ConfigReward -> Double -> Reward St
fromDouble config r = Reward (base - scale * r)
  where
    base = configRewardBaseline config
    scale = configRewardScale config


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


instance RewardFuture St where
  type StoreType St = (ConfigReward, Double, [OrderId])
  applyState = applyFutureReward

instance RewardFuture StSerialisable where
  type StoreType StSerialisable = (ConfigReward, Double, [OrderId])
  applyState = error "applyFutureReward should not be called for StSerialisable"


mkFutureReward :: ConfigReward -> RewardInFutureType -> SimSim -> SimSim -> Reward St
mkFutureReward config ByOrderPoolOrders sim _ = RewardFuture (config, 0, map orderId $ simOrdersOrderPool sim)
mkFutureReward config ByReleasedOrders sim sim' = RewardFuture (config, 0, opOrdsSim \\ opOrdsSim')
  where opOrdsSim = map orderId (simOrdersOrderPool sim)
        opOrdsSim' = map orderId (simOrdersOrderPool sim')


applyFutureReward :: StoreType St -> St -> Reward St
applyFutureReward (config, acc, []) _ = fromDouble config acc
applyFutureReward (config, acc, orderIds) (St sim _ _ _)
  | null shippedOrders = RewardFuture (config, acc, orderIds)
  | length orderIds == length shippedOrders = fromDouble config acc'
  | otherwise = RewardFuture (config, acc', orderIds \\ map orderId shippedOrders)
  where
    shippedOrders = filter ((`elem` orderIds) . orderId) (simOrdersShipped sim)
    acc' = acc + sum (map (calcRewardShipped sim) shippedOrders)


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


