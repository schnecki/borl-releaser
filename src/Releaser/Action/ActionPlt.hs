{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Releaser.Action.ActionPlt
    ( actionsPLT
    , ActionConfig (..)
    ) where

import           ClassyPrelude                     (when)
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Trans.Reader
import           Data.Foldable                     (toList)
import           Data.List                         (nub)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromJust, isJust)
import qualified Data.Text                         as T
import           Text.Printf

import           ML.BORL
import           SimSim                            hiding (productTypes)

import           Releaser.Action.Type
import           Releaser.Release.ReleasePlt
import           Releaser.Reward
import           Releaser.Reward.Type
import           Releaser.SettingsConfigParameters
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

action :: [Time] -> Reader ActionConfig (St -> IO (Reward St, St, EpisodeEnd))
action pltChange =
  return $ \(St sim incomingOrders rewardFun plts) -> do
    -- let periodLen = simPeriodLength sim
    -- let currentTime = simCurrentTime sim
    let pltsChangeMap = M.fromList $ zip productTypes pltChange
        pltsNew = M.unionWith (+) plts pltsChangeMap
    let simReleaseSet
          --  | isJust useHeuristicToFillReplMem && fromIntegral (currentTime `div` periodLen) < (nnConfig ^. replayMemoryMaxSize) = sim { simRelease = fromJust useHeuristicToFillReplMem }
          | uniqueReleaseName (simRelease sim) == pltReleaseName = sim {simRelease = mkReleasePLT pltsNew}
          | otherwise = sim
    sim' <- simulateUntil (simCurrentTime sim + periodLength) simReleaseSet incomingOrders
    let reward = mkReward rewardFun sim sim'
    newIncomingOrders <- generateOrders sim'
    writeFiles sim sim'
    return (reward, St sim' newIncomingOrders rewardFun pltsNew, False)


writeFiles :: SimSim -> SimSim -> IO ()
writeFiles sim sim' = do
  let periodLen = simPeriodLength sim
  let currentTime = simCurrentTime sim
  let period = timeToDouble (currentTime / periodLen)
  let p = show period ++ "\t"
      lb = "\n"
  let costsShipped = rewardValue $ mkReward (RewardShippedSimple config) sim sim'
  let costsPeriodEnd = rewardValue $ mkReward (RewardPeriodEndSimple config) sim sim'
  let fileCosts = "costs"
  when (currentTime == 0) $ writeFile fileCosts "Period\tCostsPeriodEnd\tCostShipped\n"
  appendFile fileCosts (p ++ show costsPeriodEnd ++ "\t" ++ show costsShipped ++ lb)
  where
    config = ConfigReward 0 1 Nothing
