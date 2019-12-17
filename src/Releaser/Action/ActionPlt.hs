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
import           Releaser.SettingsConfigParameters
import           Releaser.SettingsDemand
import           Releaser.SettingsPeriod
import           Releaser.SettingsReward
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
    let pltsChangeMap = M.fromList $ zip productTypes pltChange
        pltsNew = M.unionWith (+) plts pltsChangeMap
    let simReleaseSet
          | uniqueReleaseName (simRelease sim) == pltReleaseName = sim {simRelease = mkReleasePLT pltsNew}
          | otherwise = sim
    simWOrders <- addAdditionalOrdersToOrderPool simReleaseSet incomingOrders
    sim' <- simulateUntil (simCurrentTime simWOrders + periodLength) simWOrders [] -- are set above
    let reward = mkReward rewardFun simWOrders sim'
    newIncomingOrders <- generateOrders sim'
    writeFiles pltsNew simWOrders sim'
    return (reward, St sim' newIncomingOrders rewardFun pltsNew, False)


writeFiles :: M.Map ProductType Time -> SimSim -> SimSim -> IO ()
writeFiles plts' sim sim' = do
  let periodLen = simPeriodLength sim
  let currentTime = simCurrentTime sim
  let period = timeToDouble (currentTime / periodLen)
  let p = show period ++ sep
      sep = "\t"
      lb = "\n"
  let costsShipped = rewardValue $ mkReward (RewardShippedSimple config) sim sim'
  let costsPeriodEnd = rewardValue $ mkReward (RewardPeriodEndSimple config) sim sim'
  let fileCosts = "costs"
      filePlts = "plts"
  when (currentTime == 0) $ do
    writeFile fileCosts "Period\tCostsPeriodEnd\tCostShipped\n"
    writeFile filePlts ("Period" ++ concatMap (\p -> sep <> "\"" <> show p <> "\"") (M.keys plts') ++ lb)
  appendFile fileCosts (p ++ show costsPeriodEnd ++ "\t" ++ show costsShipped ++ lb)
  appendFile filePlts (p ++ concatMap (\p -> sep <> show p) (M.elems plts') ++ lb)
  where
    config = ConfigReward 0 1 Nothing
