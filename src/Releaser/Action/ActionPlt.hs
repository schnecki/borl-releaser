{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Releaser.Action.ActionPlt
    ( actionsPLT
    , ActionConfig (..)
    ) where

import           ClassyPrelude               (when)
import           Control.DeepSeq
import           Control.Monad.Trans.Reader
import           Data.Foldable               (toList)
import           Data.List                   (nub)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Text.Printf

import           ML.BORL
import           SimSim                      hiding (productTypes)

import           Releaser.Action.Type
import           Releaser.Costs.Type
import           Releaser.Release.ReleasePlt
import           Releaser.SettingsCosts
import           Releaser.SettingsDemand
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type


actionsPLT :: St -> Reader ActionConfig (ListOfActions, [Action St])
actionsPLT (St sim _ _ _) = do
  lowerBound <- asks configActLower
  upperBound <- asks configActUpper
  let actionList =
        map (map ((* periodLength) . fromInteger)) $
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
  return $ Action actionFun (T.pack $ filter (/= '"') $ show $ map (\x -> if x < 0 then printInt x else "+" <> printInt x) act)

  where printInt :: Time -> String
        printInt = printf "%.2f" . timeToDouble

action :: [Time] -> Reader ActionConfig (St -> IO (Reward, St, EpisodeEnd))
action pltChange =
  return $ \(St sim incomingOrders rewardFun plts) -> do
    let pltsChangeMap = M.fromList $ zip productTypes pltChange
        pltsNew = M.unionWith (+) plts pltsChangeMap
    let simReleaseSet
          | uniqueReleaseName (simRelease sim) == pltReleaseName = sim {simRelease = mkReleasePLT pltsNew}
          | otherwise = sim
    sim' <- simulateUntil (simCurrentTime sim + periodLength) simReleaseSet incomingOrders
    let (reward, rewardFun') = mkReward rewardFun sim sim'
    newIncomingOrders <- generateOrders sim'
    writeFiles sim sim'
    return (50-0.2*reward, St sim' newIncomingOrders rewardFun' pltsNew, False)

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


writeFiles :: SimSim -> SimSim -> IO ()
writeFiles sim sim' = do
  let periodLen = simPeriodLength sim
  let currentTime = simCurrentTime sim
  let period = timeToDouble (currentTime / periodLen)
  let p = show period ++ "\t"
      lb = "\n"
  let rewardShipped = fst $ mkReward RewardShippedSimple sim sim'
  let rewardPeriodEnd = fst $ mkReward RewardPeriodEndSimple sim sim'
  let fileReward = "reward"

  when (currentTime == 0) $ writeFile fileReward "Period\tRewardPeriodEnd\tRewardShipped\n"
  appendFile fileReward (p ++ show rewardPeriodEnd ++ "\t" ++ show rewardShipped ++ lb)


