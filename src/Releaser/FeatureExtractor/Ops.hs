{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Ops
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , ReduceValues
    , featExtractorSimple
    , featExtractorFullWoMachines
    , featExtractorWipAsQueueCounters
    ) where

import           Data.List                      (find, foldl', genericLength)

import qualified Data.Map                       as M


import           ML.BORL                        hiding (FeatureExtractor)
import           SimSim                         hiding (productTypes, queues)

import           Releaser.ActionFilter.Type
import           Releaser.FeatureExtractor.Type
import           Releaser.SettingsActionFilter
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type

import           Debug.Trace

type ReduceValues = Bool


featExtractorSimple :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimple useReduce = ConfigFeatureExtractor "PLTS-OP-Shipped aggregated over product types" featExt
  where
    doIf prep f
      | prep = f
      | otherwise = id
    featExt (St sim _ _ plts) =
      Extraction
        (map (doIf useReduce (scaleValue (1, 7)) . timeToDouble) (M.elems plts))
        [map reduce $ mkFromList (simOrdersOrderPool sim)] -- TODO: split also by product type
        []
        []
        [map reduce $ map genericLength (sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime (simOrdersShipped sim))]

      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)
            reduce x | useReduce = scaleValue (0, 12) x
                     | otherwise = x

featExtractorWipAsQueueCounters :: ReduceValues -> ConfigFeatureExtractor
featExtractorWipAsQueueCounters useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-FGI-Shipped" featExt
  where
    doIf prep f
      | prep = f
      | otherwise = id
    featExt (St sim _ _ plts) =
      Extraction
        (map (doIf useReduce (scaleValue (1, 7)) . timeToDouble) (M.elems plts))
        (foreachPt (map reduce . mkFromList) (simOrdersOrderPool sim))
        (M.elems $ fmap (\xs -> foreachPt (return . reduce . fromIntegral . length) xs) (simOrdersQueue sim))
        (foreachPt (map reduce . mkFromList) (simOrdersFgi sim))
        (foreachPt (map (reduce . genericLength) . sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime) (simOrdersShipped sim))
      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)
            reduce x | useReduce = scaleValue (0, 12) x
                     | otherwise = x
            foreachPt f xs = map (\pt -> f (filter ((==pt) . productType) xs)) productTypes


featExtractorFullWoMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWoMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-FGI-Shipped" featExt
  where
    doIf prep f
      | prep = f
      | otherwise = id
    featExt (St sim _ _ plts) =
      Extraction
        (map (doIf useReduce (scaleValue (1, 7)) . timeToDouble) (M.elems plts))
        (foreachPt (map reduce . mkFromList) (simOrdersOrderPool sim))
        (M.elems $ fmap (\xs -> foreachPt (map reduce . mkFromList) xs) (simOrdersQueue sim))
        (foreachPt (map reduce . mkFromList) (simOrdersFgi sim))
        (foreachPt (map (reduce . genericLength) . sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime) (simOrdersShipped sim))
      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)
            reduce x | useReduce = scaleValue (0, 12) x
                     | otherwise = x
            foreachPt f xs = map (\pt -> f (filter ((==pt) . productType) xs)) productTypes


------------------------------ Helper function ----------------------------------------

type PeriodMin = Integer
type PeriodMax = Integer


sortByTimeUntilDue :: PeriodMin -> PeriodMax -> CurrentTime -> [Order] -> [[Order]]
sortByTimeUntilDue min max currentTime = M.elems . foldl' sortByTimeUntilDue' startMap
  where
    def = fromIntegral min - periodLength
    -- startMap = M.fromList $ (map (\pt -> (pt,[])) (def : ptTypes) )
    lookup = [fromIntegral min * periodLength,fromIntegral min * periodLength + periodLength .. fromIntegral max * periodLength]
    startMap = M.fromList $ zip (def : lookup) (repeat [])
    sortByTimeUntilDue' m order =
      case find (== (dueDate order - currentTime)) lookup of
        Nothing -> M.insertWith (++) def [order] m
        Just k  -> M.insertWith (++) k [order] m


-- testSort :: IO ()
-- testSort = do
--   let xs = sortByTimeUntilDue 1 7 7 [newOrder (Product 1) 0 7,
--                                      newOrder (Product 1) 0 7
--                                     ]
--   print $ map (map orderId) xs

