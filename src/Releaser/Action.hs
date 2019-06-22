{-# LANGUAGE ViewPatterns #-}


module Releaser.Action
    ( actionsPLT
    , ActionConfig (..)
    ) where

import           ClassyPrelude              (tshow)
import           Control.DeepSeq
import           Control.Monad.Trans.Reader
import           Data.Foldable              (toList)
import           Data.List                  (nub)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)

import           ML.BORL
import           SimSim

import           Releaser.Costs
import           Releaser.Demand
import           Releaser.ReleasePLT
import           Releaser.Type

data ActionConfig = ActionConfig
  { lowerActionBound :: Integer
  , upperActionBound :: Integer
  , periodLength     :: Time
  , productTypes     :: [ProductType] -- ^ Product types must be sorted!
  }


actionsPLT :: St -> Reader ActionConfig (ListOfActions, [Action St])
actionsPLT (St sim _ _ _) = do
  lowerBound <- asks lowerActionBound
  upperBound <- asks upperActionBound
  perLen <- asks periodLength
  let actionList =
        map (map ((* perLen) . fromInteger)) $
        foldl (combs [lowerBound,lowerBound+1,upperBound]) [] [1..length pts]
  acts <- mapM mkAction actionList
  return (actionList, acts)
  where pts = nub $ fmap (fst.fst) $ toList $ simRouting sim


combs :: (NFData a) => [a] -> [[a]] -> b ->  [[a]]
combs (force -> base) [] _             = [[b] | b <- base]
combs (force -> base) (force -> acc) _ = concat [ map (b:) acc | b <- base]


mkAction :: [Time] -> Reader ActionConfig (Action St)
mkAction act = do
  actionFun <- action act
  return $ Action actionFun (tshow act)


action :: [Time] -> Reader ActionConfig (St -> IO (Reward, St, EpisodeEnd))
action pltChange = do
  perLen <- asks periodLength
  prodTypes <- asks productTypes
  return $ \(St sim incomingOrders rewardFun plts) -> do
    let pltsChangeMap = M.fromList $ zip prodTypes pltChange
        pltsNew = M.unionWith (+) plts pltsChangeMap
    let simReleaseSet
          | uniqueReleaseName (simRelease sim) == pltReleaseName = sim {simRelease = mkReleasePLT pltsNew}
          | otherwise = sim
    sim' <- simulateUntil (simCurrentTime sim + perLen) simReleaseSet incomingOrders
    let (reward, rewardFun') = mkReward rewardFun sim sim'
    newIncomingOrders <- generateOrders sim'

    return (reward, St sim' [] rewardFun' pltsNew, False)

type SimT = SimSim
type SimTPlus1 = SimSim

mkReward :: RewardFunction -> SimT -> SimTPlus1 -> (Reward, RewardFunction)
mkReward RewardShippedSimple _ sim = (sum $ map (calcRewardShipped sim) (simOrdersShipped sim), RewardShippedSimple)
mkReward RewardPeriodEndSimple _ sim = (nrWipOrders * wipCosts costConfig + nrFgiOrders * fgiCosts costConfig + nrBoOrders * boCosts costConfig, RewardPeriodEndSimple)
  where
    currentTime = simCurrentTime sim
    nrWipOrders = fromIntegral $ length (M.elems (simOrdersQueue sim)) + length (M.elems (simOrdersMachine sim))
    nrFgiOrders = fromIntegral $ length (simOrdersFgi sim)
    allOrdersInTheSystem = simOrdersOrderPool sim ++ concat (M.elems (simOrdersQueue sim)) ++ map fst (M.elems (simOrdersMachine sim)) ++ simOrdersFgi sim
    isBackorder order = currentTime >= dueDate order
    boOrders = filter isBackorder allOrdersInTheSystem
    nrBoOrders = fromIntegral $ length boOrders


calcRewardShipped :: SimSim -> Order -> Reward
calcRewardShipped sim order =
  let fromM = fromMaybe (error $ "calculating reward from order: " ++ show order)
      periodLen = fromTime (simPeriodLength sim)
      periodsLate = fromTime $ fromM (shipped order) - dueDate order
      bo = boCosts costConfig * fromRational (periodsLate / periodLen)
      periodsInProd = fromTime $ fromM (prodEnd order) - fromM (prodStart order)
      wip = wipCosts costConfig * fromRational (periodsInProd / periodLen)
      periodsInFgi = fromTime $ fromM (shipped order) - fromM (prodEnd order)
      fgi = fgiCosts costConfig * fromRational (periodsInFgi / periodLen)
   in wip + fgi + bo


