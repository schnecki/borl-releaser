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
    , featExtractorFullMachinesToQueue
    , featExtractorFullMachinesToQueueNbnBn
    , featExtractorFullWithMachines
    ) where

import           Control.DeepSeq                (rnf1)
import           Data.List                      (find, foldl', genericLength)
import qualified Data.Map                       as M


import           SimSim                         hiding (productTypes)

import           Releaser.ActionFilter.Type
import           Releaser.FeatureExtractor.Type
import           Releaser.SettingsActionFilter
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type

type ReduceValues = Bool


maxBackorderPeriod :: Integer
maxBackorderPeriod = 3


featExtractorSimple :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimple useReduce = ConfigFeatureExtractor "PLTS-OP-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        []
        []
        []
        [mkShippedDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim

featExtractorSimpleWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        []
        []
        [mkShippedDueList currentTime (simOrdersShipped sim)]
        useReduce
      where currentTime = simCurrentTime sim

featExtractorSimpleWipWithQueueCounts :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWipWithQueueCounts useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . fromIntegral . length) (M.elems $ simOrdersQueue sim))
        []
        [mkFgiList currentTime (simOrdersFgi sim)]
        [mkShippedDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim

featExtractorSimpleWipWithQueueCountsAndMachineCount :: ReduceValues -> ConfigFeatureExtractor
featExtractorSimpleWipWithQueueCountsAndMachineCount useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-MachineCounter-Shipped aggregated over product types" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        [mkOrderPoolList currentTime (incOrds ++ simOrdersOrderPool sim)]
        (map (return . return . genericLength) (M.elems $ simOrdersQueue sim))
        [[sum $ foreachMachine genericLength (M.toList (simOrdersMachine sim))]]
        [mkFgiList currentTime (simOrdersFgi sim)]
        [mkShippedDueList currentTime (simOrdersShipped sim)]
        useReduce
      where
        currentTime = simCurrentTime sim
        foreachMachine f xs = map (\machine -> f . map (fst . snd) $ filter ((== machine) . fst) xs) machines

featExtractorWipAsQueueCounters :: ReduceValues -> ConfigFeatureExtractor
featExtractorWipAsQueueCounters useReduce = ConfigFeatureExtractor "PLTS-OP-QueueCounters-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt (return . fromIntegral . length)) (simOrdersQueue sim))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkShippedDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes


featExtractorFullWoMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWoMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (simOrdersQueue sim))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkShippedDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes


featExtractorFullMachinesToQueue :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullMachinesToQueue useReduce = ConfigFeatureExtractor "PLTS-OP-(Queues+Machines)-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (foldl' (\m (b, (o, _)) -> M.insertWith (++) b [o] m) (simOrdersQueue sim) (M.toList $ simOrdersMachine sim)))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkShippedDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes

featExtractorFullMachinesToQueueNbnBn :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullMachinesToQueueNbnBn useReduce = ConfigFeatureExtractor "PLTS-OP-(Queues+Machines)-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (foldl' (\m (b, (o, _)) -> M.insertWith (++) b [o] m) (simOrdersQueue sim) (M.toList $ simOrdersMachine sim)))
        []
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkShippedDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = groupPts $ map (\pt -> f (filter ((== pt) . productType) xs)) productTypes
        groupPts [p1, p2, p3, p4, p5, p6] = let xs =  [p1 +. p3 +. p4 +. p6, p2 +. p5] in rnf1 xs `seq` xs
        groupPts _ = error "Wrong featureExtractor configuration, used featExtractorFullMachinesToQueueNbnBn but don't have 6 Products"
        (+.) = zipWith (+)


featExtractorFullWithMachines :: ReduceValues -> ConfigFeatureExtractor
featExtractorFullWithMachines useReduce = ConfigFeatureExtractor "PLTS-OP-Queues-Machines-FGI-Shipped" featExt
  where
    featExt (St sim incOrds _ plts) =
      Extraction
        (map (realToFrac . timeToDouble) (M.elems plts))
        (foreachPt (mkOrderPoolList currentTime) (incOrds ++ simOrdersOrderPool sim))
        (M.elems $ fmap (foreachPt mkFromList) (simOrdersQueue sim))
        (foreachMachine (mkUntilDueList currentTime) (M.toList (simOrdersMachine sim)))
        (foreachPt (mkFgiList currentTime) (simOrdersFgi sim))
        (foreachPt (mkShippedDueList currentTime) (simOrdersShipped sim))
        useReduce
      where
        currentTime = simCurrentTime sim
        mkFromList = mkUntilDueList currentTime
        foreachPt f xs = map (\pt -> f (filter ((== pt) . productType) xs)) productTypes
        foreachMachine f xs = map (\machine -> f . map (fst . snd) $ filter ((== machine) . fst) xs) machines

test =
  [ newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600864.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600864.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600866.0
  , newOrder (Product 1) 0 600865.0
  , newOrder (Product 1) 0 600866.0
  ]

mkShippedDueList :: CurrentTime -> [Order] -> [Float]
mkShippedDueList t =
  init .
  map genericLength . sortByTimeUntilDue (-maxBackorderPeriod) 0 t

mkOrderPoolList :: CurrentTime -> [Order] -> [Float]
mkOrderPoolList t =
 tail .
 map genericLength . sortByTimeUntilDue (configActFilterMin actionFilterConfig) (configActFilterMax actionFilterConfig) t

mkFgiList :: CurrentTime -> [Order] -> [Float]
-- mkFgiList t = init . tail . mkUntilDueList t
mkFgiList t =
  init . tail .
  map genericLength . sortByTimeUntilDue 1 (configActFilterMax actionFilterConfig) t

mkUntilDueList :: CurrentTime -> [Order] -> [Float]
mkUntilDueList t xs = map genericLength (sortByTimeUntilDue (-maxBackorderPeriod) (configActFilterMax actionFilterConfig) t xs)


------------------------------ Helper function ----------------------------------------

type PeriodMin = Integer
type PeriodMax = Integer


sortByTimeUntilDue :: PeriodMin -> PeriodMax -> CurrentTime -> [Order] -> [[Order]]
sortByTimeUntilDue min max currentTime = M.elems . foldl' sortByTimeUntilDue' startMap
  where
    def = fromIntegral min - periodLength
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

-- testSortBackorderDueList :: CurrentTime -> IO ()
-- testSortBackorderDueList t = do
--   let ords = [newOrder (Product 1) 0 7, newOrder (Product 1) 0 7]
--   let xs = mkShippedDueList t ords
--   print xs
--   let ys = map genericLength (sortByTimeUntilDue (-maxBackorderPeriod) (configActFilterMax actionFilterConfig) t ords)
--   print ys
