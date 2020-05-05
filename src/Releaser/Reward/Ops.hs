{-# LANGUAGE BangPatterns      #-}
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
  , configReward100
  , configReward50
  , configReward250
  , configReward500
  , configReward2500
  ) where

import           Control.DeepSeq
import           Data.Function                 (on)
import           Data.List                     (groupBy, sortBy, (\\))
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe)
import           Data.Serialize
import           GHC.Generics

import           Debug.Trace

import           ML.BORL
import           SimSim

import           Releaser.Reward.Type
import           Releaser.SettingsActionFilter
import           Releaser.SettingsCosts
import           Releaser.SettingsPeriod
import           Releaser.Type


-- Note: the average reward is then also only in (-1,1)
configRewardPosNeg1 :: ConfigReward
configRewardPosNeg1 = ConfigReward 1 (1/277.8) (Just $ -1)

configRewardFutureOpOrds :: ConfigReward
configRewardFutureOpOrds = ConfigReward 50 0.5 (Just $ -50)

configRewardPeriodEnd :: ConfigReward
configRewardPeriodEnd = ConfigReward 50 0.25 (Just $ -50)

configReward100 :: ConfigReward
configReward100 = ConfigReward 100 1 (Just $ -100)

configReward50 :: ConfigReward
configReward50 = ConfigReward 50 1 (Just $ -50)

configReward250 :: ConfigReward
configReward250 = ConfigReward 250 1 (Just $ -250)


configReward500 :: ConfigReward
configReward500 = ConfigReward 500 1 (Just $ -500)

configReward2500 :: ConfigReward
configReward2500 = ConfigReward 2500 1 (Just $ -2500)


type SimT = SimSim
type SimTPlus1 = SimSim

-- | This scales the reward and ensures the result to be in the range [-baseline, baseline].
fromDouble :: ConfigReward -> Double -> Reward St
fromDouble (ConfigRewardCosts Nothing) r = Reward $ realToFrac r
fromDouble (ConfigRewardCosts (Just maxVal)) r = Reward $ realToFrac $ min maxVal r
fromDouble config r = Reward $ realToFrac $ maxFun (base - scale * r)
  where
    base = configRewardBaseline config
    scale = configRewardScale config
    maxFun = case configRewardMinimum config of
      Nothing     -> id
      Just minVal -> max minVal


mkReward :: RewardFunction -> SimT -> SimTPlus1 -> Reward St
mkReward (RewardShippedSimple config) _ sim = fromDouble config $ sum $ map (calcRewardShipped sim) (simOrdersShipped sim)
mkReward (RewardPeriodEndSimple config) _ sim =
  -- trace ("(wip, fgi, bo): " ++ show (nrWipOrders * wipCosts costConfig, nrFgiOrders * fgiCosts costConfig, nrBoOrders * boCosts costConfig))
  -- trace ("sum " ++ show (nrWipOrders * wipCosts costConfig+ nrFgiOrders * fgiCosts costConfig+ nrBoOrders * boCosts costConfig))

  fromDouble config (nrWipOrders * wipCosts costConfig + nrFgiOrders * fgiCosts costConfig + nrBoOrders * boCosts costConfig)
  where
    currentTime = simCurrentTime sim
    nrWipOrders = fromIntegral $ length (concat $ M.elems (simOrdersQueue sim)) + M.size (simOrdersMachine sim)
    nrFgiOrders = fromIntegral $ length (simOrdersFgi sim)
    allOrdersInTheSystem = simOrdersOrderPool sim ++ concat (M.elems (simOrdersQueue sim)) ++ map fst (M.elems (simOrdersMachine sim)) ++ simOrdersFgi sim
    isBackorder order = currentTime >= dueDate order
    boOrders = filter isBackorder allOrdersInTheSystem
    nrBoOrders = fromIntegral $ length boOrders
mkReward (RewardInFuture config futureType) sim sim' = mkFutureReward config futureType sim sim'

data Future = Future
  { nrOfOrders  :: !Int
  , accumulator :: !Double
  , orderIds    :: ![OrderId]
  } deriving (Generic, Serialize, NFData, Show)

instance RewardFuture St where
  type StoreType St = (ConfigReward, [Future])
  applyState = applyFutureReward

instance RewardFuture StSerialisable where
  type StoreType StSerialisable = (ConfigReward, [Future])
  applyState = error "applyFutureReward should not be called for StSerialisable"


mkFutureReward :: ConfigReward -> RewardInFutureType -> SimSim -> SimSim -> Reward St
mkFutureReward config ByOrderPoolOrders sim sim' = RewardFuture (config, map (\os -> Future (length os) 0 (map orderId os)) orders)
  where
    orders = groupBy ((==) `on` dueDate) $ sortBy (compare `on` dueDate) $ simOrdersOrderPool sim'

    -- orders = map (\dd -> filter ((== dd) . dueDate) (simOrdersOrderPool sim)) [minPLT, minPLT+periodLen .. maxPLT]
    -- currentTime = simCurrentTime sim
    -- periodLen = simPeriodLength sim
    -- ActionFilterConfig minPLTPeriod maxPLTPeriod = actionFilterConfig
    -- minPLT = currentTime + fromIntegral minPLTPeriod * periodLength
    -- maxPLT = currentTime + fromIntegral maxPLTPeriod * periodLength

mkFutureReward config ByReleasedOrders sim sim'
  | null orders = RewardEmpty
  | otherwise = RewardFuture (config, [Future (length orders) 0 orders])
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
  | all (null . orderIds) futures' = finalize futures'
  | otherwise = RewardFuture (config, futures')
  where
    -- finalize futures =
    --   -- average per (filled) bucket
    --   avgRew $ map (\(Future _ acc _) -> fromDouble config acc) (filter ((> 0) . nrOfOrders) futures)
    -- avgRew xs = Reward $ sum (map rewardValue xs) / fromIntegral (max 1 (length xs))
    finalize futures =
      -- 1. -- average costs per order (results in minimizing number of orders by releasing one product immediately and the other as late as possible)
      -- fromDouble config $ (\xs -> sum (map (\(Future _ acc _) -> acc) xs) / sum (map (\(Future nr _ _) -> fromIntegral nr) xs)) (filter ((> 0) . nrOfOrders) futures)

      -- average costs per order (results in minimizing number of orders by releasing one product immediately and the other as late as possible)
      fromDouble config $ (\xs -> sum (map (\(Future _ acc _) -> acc) xs) -- / sum (map (\(Future nr _ _) -> fromIntegral nr) xs)
                          ) (filter ((> 0) . nrOfOrders) futures)

    shippedOrders = map (\(Future _ _ orderIds) -> filter ((`elem` orderIds) . orderId) (simOrdersShipped sim)) futures
    futures' = zipWith updateFuture futures shippedOrders
    updateFuture (Future count acc openOrders) shippedOrds =
      let acc' = acc + sum (map (calcRewardShipped sim) shippedOrds)
      in Future count acc' (openOrders \\ map orderId shippedOrds)
-- fromDouble config $ sum $ map (calcRewardShipped sim) (simOrdersShipped sim)

-- fromDouble config (nrWipOrders * wipCosts costConfig + nrFgiOrders * fgiCosts costConfig + nrBoOrders * boCosts costConfig)

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
   in
    -- trace (show order ++ " (wip,fgi,bo): " ++ show (wip, fgi, bo))
    wip + fgi + bo


