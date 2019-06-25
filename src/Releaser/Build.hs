{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Releaser.Build
    ( buildBORLTable
    , buildBORLTensorflow
    , buildSim
    , periodLength
    , ptTypes
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.Function                   (on)
import           Data.List                       (foldl', genericLength, groupBy, nub,
                                                  sort, sortBy)
import           Data.List                       (find)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe)
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Random
import           Text.Printf

-- ANN modules
import           Grenade
import qualified TensorFlow.Core                 as TF hiding (value)
import qualified TensorFlow.GenOps.Core          as TF (abs, add, approximateEqual,
                                                        approximateEqual, assign, cast,
                                                        getSessionHandle, getSessionTensor,
                                                        identity', lessEqual, matMul, mul,
                                                        readerSerializeState, relu, relu',
                                                        shape, square, sub, tanh, tanh',
                                                        truncatedNormal)
import qualified TensorFlow.Minimize             as TF


import           Experimenter                    hiding (sum)
import qualified Experimenter                    as E
import           ML.BORL                         as B
import           SimSim

import           Releaser.Action
import           Releaser.ActionFilter
import           Releaser.Costs
import           Releaser.Demand
import           Releaser.ReleasePLT
import           Releaser.Type

periodLength :: Time
periodLength = 1


buildSim :: IO SimSim
buildSim = newSimSimIO routing procTimes periodLength (releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)]))
           --releaseImmediate
           -- (mkReleasePLT plts)

           dispatchFirstComeFirstServe shipOnDueDate
  where plts = M.fromList $ zip ptTypes [1..]

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))
                        ,(Product 2, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))])
            ,(Machine 2,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130/960) (170/960)))])
            ,(Machine 3,[(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180/960) (200/960)))])
            ]

routing :: Routes
routing =
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 1   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 2, Machine 1) --> Queue 3
  , (Product 2, Queue 3)   --> Machine 3
  , (Product 2, Machine 3) --> FGI
  ]

ptTypes :: [ProductType]
ptTypes = sort $ nub $ map (fst . fst) routing


testDemand :: IO ()
testDemand = do
  let nr = 1000
  sim <- buildSim
  xs <- replicateM nr (generateOrders sim)
  let len = fromIntegral $ length (concat xs)
  putStr "Avg order slack time: "
  print $ timeToDouble (sum $ map orderSlackTime (concat xs)) / len
  putStr "Avg order arrival date: "
  print $ timeToDouble (sum $ map arrivalDate (concat xs)) / len
  putStr "Avg number of order per period: "
  print $ fromIntegral (sum (map length xs)) / fromIntegral nr
  putStr "Avg order due date: "
  print $ timeToDouble (sum $ map dueDate (concat xs)) / len
  putStr "Avg number of order per product type"
  print $ map length $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)
  print $ map (productType . head) $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)

------------------------------------------------------------
--------------------------- BORL ---------------------------
------------------------------------------------------------

actionConfig :: ActionConfig
actionConfig = ActionConfig
  { actLowerActionBound = -1
  , actUpperActionBound = 1
  , actPeriodLength     = periodLength
  , actProductTypes     = ptTypes
  }

actionFilterConfig :: ActionFilterPLTConfig
actionFilterConfig = ActionFilterPLTConfig
  { actFilMinimumPLT   = 1
  , actFilMaximumPLT   = 7
  , actFilPeriodLength = periodLength
  }

-- | BORL Parameters.
borlParams :: Parameters
borlParams = Parameters
  { _alpha            = 0.5
  , _beta             = 0.05
  , _delta            = 0.04
  , _gamma            = 0.30
  , _epsilon          = 0.075
  , _exploration      = 0.8
  , _learnRandomAbove = 0.0
  , _zeta             = 1.0
  , _xi               = 0.2
  }

-- | Decay function of parameters.
decay :: Decay
decay t  p@(Parameters alp bet del ga eps exp rand zeta xi)
  | t `mod` 300 == 0 =
    Parameters
      (max 0.03 $ slow * alp)
      (max 0.015 $ slow * bet)
      (max 0.015 $ slow * del)
      (max 0.01 $ slow * ga)
      (max 0.05 $ slow * eps) -- (0.5*bet)
      (max 0.10 $ slower * exp)
      rand
      zeta -- zeta
      -- (max 0.075 $ slower * xi)
      (0.5*bet)
  | otherwise = p
  where
    slower = 0.995
    slow = 0.98
    faster = 1.0 / 0.99
    f = max 0.01


-- SuperSimple: AggregatedOverProductTypes - OrderPool+Shipped

netInpPre :: St -> [[Double]]
netInpPre (St sim _ _ plts) =
  [ map (scaleValue (1, 7) . timeToDouble) (M.elems plts)
  , map reduce $ mkFromList (simOrdersOrderPool sim) -- TODO: split also by product type
  , map reduce $ map genericLength (sortByTimeUntilDue (-actFilMaximumPLT actionFilterConfig) 0 currentTime (simOrdersShipped sim))
  ]
  where
    currentTime = simCurrentTime sim
    mkFromList xs = map genericLength (sortByTimeUntilDue (actFilMinimumPLT actionFilterConfig) (actFilMaximumPLT actionFilterConfig) currentTime xs)
    reduce x = scaleValue (0, 12) x
        -- reduce x = fromIntegral $ ceiling (x / 3)

netInp :: St -> [Double]
netInp = concat . netInpPre


nnConfig :: NNConfig St
nnConfig =
  NNConfig
    { _toNetInp = netInp
    , _replayMemoryMaxSize = 5000
    , _trainBatchSize = 128
    , _grenadeLearningParams = LearningParameters 0.01 0.9 0.0001
    , _prettyPrintElems = ppSts
    , _scaleParameters = scalingByMaxAbsReward False 20
    , _updateTargetInterval = 10000
    , _trainMSEMax = Just $ 1 / 100 * 3
    }
  where
    len =
      length ptTypes + 1 + length [actFilMinimumPLT actionFilterConfig .. actFilMaximumPLT actionFilterConfig] + 1 +
      length [-actFilMaximumPLT actionFilterConfig .. 0]
    (lows, highs) = (replicate len (-1), replicate len 1)
    vals = zipWith (\lo hi -> map rnd [lo,lo + (hi - lo) / 3 .. hi]) lows highs
    valsRev = zipWith (\lo hi -> map rnd [hi,hi - (hi - lo) / 3 .. lo]) lows highs
    rnd x = fromIntegral (round (100 * x)) / 100
    ppSts = take 300 (combinations vals) ++ take 300 (combinations valsRev)
    combinations :: [[a]] -> [[a]]
    combinations [] = []
    combinations [xs] = map return xs
    combinations (xs:xss) = concatMap (\x -> map (x :) ys) xs
      where
        ys = combinations xss


modelBuilder :: (TF.MonadBuild m) => [Action a] -> St -> m TensorflowModel
modelBuilder actions initState =
  buildModel $
  inputLayer1D (genericLength (netInp initState)) >> fullyConnected1D 89 TF.relu' >> fullyConnected1D 20 TF.relu' >> fullyConnected1D (genericLength actions) TF.tanh' >>
  trainingByAdam1DWith TF.AdamConfig {TF.adamLearningRate = 0.001, TF.adamBeta1 = 0.9, TF.adamBeta2 = 0.999, TF.adamEpsilon = 1e-8}


type PeriodMin = Integer
type PeriodMax = Integer

instance Show St where
  show = filter (/= '"') . show . map (map printFloat) . netInpPre
    where
    printFloat :: Double -> String
    printFloat = printf "%2.0f"

-- testSort :: IO ()
-- testSort = do
--   let xs = sortByTimeUntilDue 1 7 7 [newOrder (Product 1) 0 7,
--                                      newOrder (Product 1) 0 7
--                                     ]
--   print $ map (map orderId) xs

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

buildBORLTable :: IO (BORL St)
buildBORLTable = do
  sim <- buildSim
  startOrds <- generateOrders sim
  let initSt = St sim startOrds RewardShippedSimple (M.fromList $ zip (productTypes sim) (map Time [1,1..]))
  let (actionList, actions) = mkConfig (actionsPLT initSt) actionConfig
  let actionFilter = mkConfig (actionFilterPLT actionList) actionFilterConfig
  return $ mkUnichainTabular algBORL initSt netInp actions actionFilter borlParams decay Nothing


buildBORLTensorflow :: MonadBorl (BORL St)
buildBORLTensorflow = do
  sim <- Simple buildSim
  startOrds <- Simple $ generateOrders sim
  let initSt = St sim startOrds RewardShippedSimple (M.fromList $ zip (productTypes sim) (map Time [1,1..]))
  let (actionList, actions) = mkConfig (actionsPLT initSt) actionConfig
  let actionFilter = mkConfig (actionFilterPLT actionList) actionFilterConfig
  let alg = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) (DivideValuesAfterGrowth 1000 70000) True
  mkUnichainTensorflowM alg initSt actions actionFilter borlParams decay (modelBuilder actions initSt) nnConfig Nothing

------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------

instance ExperimentDef (BORL St) where

  type ExpM (BORL St) = MonadBorl

  type Serializable (BORL St) = BORLSerialisable StSerialisable
  serialisable = toSerialisableWith serializeSt
  deserialisable =
    unsafePerformIO $
    runMonadBorl $ do
      borl <- Simple buildBORLTable
      let (St sim _ _ _) = borl ^. s
      let (_, actions) = mkConfig (actionsPLT (borl ^. s)) actionConfig
      return $
        fromSerialisableWith
          (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
          actions
          (borl ^. actionFilter)
          (borl ^. decayFunction)
          netInp
          netInp
          (modelBuilder actions (borl ^. s))
  type InputValue (BORL St) = [Order]
  type InputState (BORL St) = ()


  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput _ borl _ _ = do
    let (St _ inc _ _) = borl ^. s
    return (inc, ())


  -- ^ Run a step of the environment and return new state and result.
  -- runStep :: (MonadIO MonadBorl) => a -> InputValue a -> E.Period -> MonadBorl ([StepResult], a)
  runStep borl incOrds _ = do
    borl' <- stepM (set (s.nextIncomingOrders) incOrds borl)

    -- helpers
    let simT = timeToDouble $ simCurrentTime $ borl' ^. s.simulation
    let borlT = borl' ^. t

    -- demand
    let demand = StepResult "demand" (Just simT) (fromIntegral $ length $ borl ^. s.nextIncomingOrders)

    -- cost related measures
    let (StatsOrderCost earnOld wipOld boOld fgiOld) = simStatsOrderCosts $ simStatistics (borl ^. s.simulation)
    let (StatsOrderCost earn wip bo fgi) = simStatsOrderCosts $ simStatistics (borl' ^. s.simulation)
    let cEarn = StepResult "EARN" (Just simT) (fromIntegral (earn - earnOld))
    let cBoc  = StepResult "BOC" (Just simT) (boCosts costConfig *  fromIntegral (bo - boOld))
    let cWip  = StepResult "WIPC" (Just simT) (wipCosts costConfig *  fromIntegral (wip - wipOld))
    let cFgi  = StepResult "FGIC" (Just simT) (fgiCosts costConfig *  fromIntegral (fgi - fgiOld))
    let cSum = StepResult "SUMC" (Just simT) (cBoc ^. resultYValue + cWip ^. resultYValue + cFgi ^. resultYValue)

    let curOp = StepResult "op" (Just simT) (fromIntegral $ length $ simOrdersOrderPool $ borl' ^. s.simulation)
    let curWip = StepResult "wip" (Just simT) (fromIntegral $ wip-wipOld)
    let curBo = StepResult "bo" (Just simT) (fromIntegral $ bo-boOld)
    let curFgi = StepResult "fgi" (Just simT) (fromIntegral $ fgi-fgiOld)

    -- time related measures
    let (StatsFlowTime ftNrFloorAndFgi (StatsOrderTime sumTimeFloorAndFgi stdDevFloorAndFgi _) mTardFloorAndFgi) = simStatsShopFloorAndFgi $ simStatistics (borl' ^. s.simulation)
    let tFtMeanFloorAndFgi = StepResult "FTMeanFloorAndFgi" (Just simT) (fromRational sumTimeFloorAndFgi / fromIntegral ftNrFloorAndFgi)
    let tFtStdDevFloorAndFgi = StepResult "FTStdDevFloorAndFgi" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloorAndFgi)
    let tTardPctFloorAndFgi = StepResult "TARDPctFloorAndFgi" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloorAndFgi ) mTardFloorAndFgi)
    let tTardMeanFloorAndFgi = StepResult "TARDMeanFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard ) mTardFloorAndFgi)
    let tTardStdDevFloorAndFgi = StepResult "TARDStdDevFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloorAndFgi)


    let (StatsFlowTime ftNrFloor (StatsOrderTime sumTimeFloor stdDevFloor _) mTardFloor) = simStatsShopFloor $ simStatistics (borl' ^. s.simulation)
    let tFtMeanFloor = StepResult "FTMeanFloor" (Just simT) (fromRational sumTimeFloor / fromIntegral ftNrFloor)
    let tFtStdDevFloor = StepResult "FTStdDevFloor" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloor)
    let tTardPctFloor = StepResult "TARDPctFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloor ) mTardFloor)
    let tTardMeanFloor = StepResult "TARDMeanFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard ) mTardFloor)
    let tTardStdDevFloor = StepResult "TARDStdDevFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard ) mTardFloor)

    -- BORL' related measures
    let avgRew = StepResult "AvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies.rho.proxyScalar)
        pltP1 = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s.plannedLeadTimes))
        pltP2 = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s.plannedLeadTimes))
        psiRho = StepResult "PsiRho" (Just $ fromIntegral borlT) (borl' ^. psis._1)
        psiV = StepResult "PsiV" (Just $ fromIntegral borlT) (borl' ^. psis._2)
        psiW = StepResult "PsiW" (Just $ fromIntegral borlT) (borl' ^. psis._3)

    return ([-- cost related measures
              cSum, cEarn, cBoc, cWip, cFgi
             -- floor
            , curOp, curWip, curBo, curFgi, demand
             -- time related measures
            , tFtMeanFloorAndFgi, tFtStdDevFloorAndFgi, tTardPctFloorAndFgi, tTardMeanFloorAndFgi, tTardStdDevFloorAndFgi
            , tFtMeanFloor, tFtStdDevFloor, tTardPctFloor, tTardMeanFloor, tTardStdDevFloor
             -- BORL related measures
            , avgRew, pltP1, pltP2, psiRho, psiV, psiW
            ], borl')


  -- ^ Provides the parameter setting.
  -- parameters :: a -> [ParameterSetup a]
  parameters _ = [ ParameterSetup "Algorithm" (set algorithm) (view algorithm) (Just $ return . const [algBORL, algVPsi, algDQN]) Nothing
                 , ParameterSetup "RewardType" (set (s.rewardFunctionOrders)) (view (s.rewardFunctionOrders)) (Just $ return . const [RewardShippedSimple, RewardPeriodEndSimple]) Nothing
                 , ParameterSetup "ReleaseAlgorithm" (\r -> over (s.simulation) (\sim -> sim { simRelease = r })) (simRelease . view (s.simulation)) (Just $ return . const [])
                 ]
    where algVPsi = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) (DivideValuesAfterGrowth 1000 70000) True


  -- ^ This function defines how to find experiments that can be resumed. Note that the experiments name is always a
  -- comparison factor, that is, experiments with different names are unequal.
  equalExperiments (borl1, st1) (borl2, st2) =
    -- st1 == st2 &&
    (borl1^.s.nextIncomingOrders,borl1^. s.rewardFunctionOrders,borl1^.s.plannedLeadTimes, borl1 ^. t, borl1 ^. episodeNrStart, borl1 ^. B.parameters, borl1 ^. algorithm, borl1 ^. phase, borl1 ^. lastVValues, borl1 ^. lastRewards, borl1 ^. psis) ==
    (borl2^.s.nextIncomingOrders,borl2^. s.rewardFunctionOrders,borl2^.s.plannedLeadTimes, borl2 ^. t, borl2 ^. episodeNrStart, borl2 ^. B.parameters, borl2 ^. algorithm, borl2 ^. phase, borl2 ^. lastVValues, borl2 ^. lastRewards, borl2 ^. psis)

