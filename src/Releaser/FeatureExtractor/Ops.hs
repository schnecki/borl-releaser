{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Ops
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , ReduceValues
    , featExtractorSimple
    , featExtractorSimpleWithQueueCounts
    , featExtractorSimpleWipWithQueueCounts
    , featExtractorSimpleWipWithQueueCountsAndMachineCount
    , featExtractorWipAsQueueCounters
    , featExtractorFullWoMachines
    , featExtractorFullWithMachines
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


maxBackorderPeriod :: Integer
maxBackorderPeriod = 2


featExtractorSimple :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimple useReduce = ConfigFeatureExtractor "PLTS-OP-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        []
        []
        []
        [mkBackorderDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim

featExtractorSimpleWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        []
        []
        [mkBackorderDueList currentTime (simOrdersShipped sim)]
        useReduce
      where currentTime = simCurrentTime sim

featExtractorSimpleWipWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWipWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        []
        [mkFgiList currentTime (simOrdersFgi sim)]
        [mkBackorderDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim

featExtractorSimpleWipWithQueueCountsAndMachineCount :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWipWithQueueCountsAndMachineCount useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-MachineCounter-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . genericLength) (M.elems $ simOrdersQueue sim))
        [[genericLength (M.elems $ simOrdersMachine sim)]]
        [mkFgiList currentTime (simOrdersFgi sim)]
        [mkBackorderDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim


featExtractorWipAsQueueCounters :: ReduceValues -> ConfigFeatureExtractor
featExtractorWipAsQueueCounters useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt (return . fromIntegral . length)) (simOrdersQueue sim))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkBackorderDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes


featExtractorFullWoMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWoMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (simOrdersQueue sim))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkBackorderDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes

featExtractorFullWithMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWithMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map timeToDouble (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (simOrdersQueue sim))
        (foreachMachine (mkUntilDueList currentTime) (M.toList (simOrdersMachine sim)))
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkBackorderDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes
        foreachMachine f xs = map (\machine -> f . map (fst . snd) $ filter ((== machine) . fst) xs) machines


mkBackorderDueList :: CurrentTime -> [Order] -> [Double]
mkBackorderDueList t xs = init $ map genericLength (sortByTimeUntilDue (-maxBackorderPeriod) 0 t xs)

mkOrderPoolList :: CurrentTime -> [Order] -> [Double]
mkOrderPoolList t = tail . mkUntilDueList t

mkFgiList :: CurrentTime -> [Order] -> [Double]
mkFgiList t = init . tail . mkUntilDueList t

mkUntilDueList :: CurrentTime -> [Order] -> [Double]
mkUntilDueList t xs = map genericLength (sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) t xs)


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

