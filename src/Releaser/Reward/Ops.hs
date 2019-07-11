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

import           ML.BORL
import           SimSim

import           Releaser.SettingsCosts
import           Releaser.Type


type SimT = SimSim
type SimTPlus1 = SimSim

data ConfigReward =
  ConfigReward
    { configRewardBaseline :: Double
    , configRewardScale    :: Double
    }

configReward :: ConfigReward
configReward = ConfigReward 50 0.2

fromDouble :: Double -> Reward St
fromDouble r = Reward (base - scale * r)
  where
    base = configRewardBaseline configReward
    scale = configRewardScale configReward


mkReward :: RewardFunction -> SimT -> SimTPlus1 -> Reward St
mkReward RewardShippedSimple _ sim = fromDouble $ sum $ map (calcRewardShipped sim) (simOrdersShipped sim)
mkReward RewardPeriodEndSimple _ sim = fromDouble $ nrWipOrders * wipCosts costConfig + nrFgiOrders * fgiCosts costConfig + nrBoOrders * boCosts costConfig
  where
    currentTime = simCurrentTime sim
    nrWipOrders = fromIntegral $ length (M.elems (simOrdersQueue sim)) + length (M.elems (simOrdersMachine sim))
    nrFgiOrders = fromIntegral $ length (simOrdersFgi sim)
    allOrdersInTheSystem = simOrdersOrderPool sim ++ concat (M.elems (simOrdersQueue sim)) ++ map fst (M.elems (simOrdersMachine sim)) ++ simOrdersFgi sim
    isBackorder order = currentTime >= dueDate order
    boOrders = filter isBackorder allOrdersInTheSystem
    nrBoOrders = fromIntegral $ length boOrders
mkReward (RewardInFuture futureType) sim sim' = mkFutureReward futureType sim sim'


instance RewardFuture St where
  type Storage St = (Double, [OrderId])
  -- applyState :: Storage St -> St -> Reward St
  applyState = applyFutureReward
  -- mapStorage :: (St -> StSerialisable) -> Storage St -> Storage StSerialisable
  mapStorage _ (nr, ords) = RewardFuture (nr, ords)

-- instance RewardFutureState StSerialisable where
--   type Storage StSerialisable = (Double, [OrderId])
--   applyState = error "applyState on StSerialisable should never be called"
--   mapStorage _ (nr, ords) = (nr, ords)

instance RewardFuture StSerialisable where
  type Storage StSerialisable = (Double, [OrderId])


mkFutureReward :: RewardInFutureType -> SimSim -> SimSim -> Reward St
mkFutureReward ByOrderPoolOrders sim _ = RewardFuture (0, map orderId $ simOrdersOrderPool sim)
mkFutureReward ByReleasedOrders sim sim' = RewardFuture (0, opOrdsSim \\ opOrdsSim')
  where opOrdsSim = map orderId (simOrdersOrderPool sim)
        opOrdsSim' = map orderId (simOrdersOrderPool sim')


applyFutureReward :: Storage St -> St -> Reward St
applyFutureReward (acc, []) _ = fromDouble acc
applyFutureReward (acc, orderIds) (St sim _ _ _)
  | null shippedOrders = RewardFuture (acc, orderIds)
  | length orderIds == length shippedOrders = fromDouble acc'
  | otherwise = RewardFuture (acc', orderIds \\ map orderId shippedOrders)
  where
    shippedOrders = filter ((`elem` orderIds) . orderId) (simOrdersShipped sim)
    acc' = acc + sum (map (calcRewardShipped sim) shippedOrders)


calcRewardShipped :: SimSim -> Order -> Double
calcRewardShipped sim order =
  let fromM = fromMaybe (error $ "calculating reward from order: " ++ show order)
      periodLen = fromTime (simPeriodLength sim)
      periodsLate = fromTime $ fromIntegral . ceiling $ fromM (shipped order) - dueDate order
      bo = boCosts costConfig * fromRational (periodsLate / periodLen)
      fullPeriodsInProd = fromTime $ fromIntegral . floor $ fromM (prodEnd order) - fromM (prodStart order)
      wip = wipCosts costConfig * fromRational (fullPeriodsInProd / periodLen)
      periodsInFgi = fromTime $ fromIntegral . floor $ fromM (shipped order) - fromM (prodEnd order)
      fgi = fgiCosts costConfig * fromRational (periodsInFgi / periodLen)
   in wip + fgi + bo
