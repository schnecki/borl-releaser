{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Ops
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , ReduceValues
    , featExtractorSimple
    , featExtractorSimpleWithQueueCounts
    , featExtractorSimpleWipWithQueueCounts
    , featExtractorFullWoMachines
    , featExtractorWipAsQueueCounters
    ) where

import           Data.Function                  (on)
import           Data.List                      (find, foldl', genericLength, groupBy,
                                                 sortBy)

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
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (timeToDouble) (M.elems plts))
        [mkFromList (incOrds ++ simOrdersOrderPool sim)] -- TODO: split also by product type
        []
        []
        [map genericLength (sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime (simOrdersShipped sim))]
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)

featExtractorSimpleWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkFromList (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        []
        [map genericLength (sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime (simOrdersShipped sim))]
        useReduce
      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)

featExtractorSimpleWipWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWipWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkFromList (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        [mkFromList (simOrdersFgi sim)]
        [map genericLength (sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime (simOrdersShipped sim))] -- WHY 0 and not 1
        useReduce

      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)


featExtractorWipAsQueueCounters :: ReduceValues -> ConfigFeatureExtractor
featExtractorWipAsQueueCounters useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        (foreachPt mkFromList (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (\xs -> foreachPt (return . fromIntegral . length) xs) (simOrdersQueue sim))
        (foreachPt mkFromList (simOrdersFgi sim))
        (foreachPt (map genericLength . sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime) (simOrdersShipped sim))
        useReduce
      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)
            foreachPt f xs = map (\pt -> f (filter ((==pt) . productType) xs)) productTypes


featExtractorFullWoMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWoMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        (foreachPt mkFromList (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (simOrdersQueue sim))
        (foreachPt mkFromList (simOrdersFgi sim))
        (foreachPt (map genericLength . sortByTimeUntilDue (-configActFilterMax actionFilterConfig) 0 currentTime) (simOrdersShipped sim))
        useReduce
      where currentTime = simCurrentTime sim
            mkFromList xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) currentTime xs)
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

