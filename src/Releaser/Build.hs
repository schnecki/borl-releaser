{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}


module Releaser.Build
    ( buildBORLTable
    , buildBORLTensorflow
    , buildSim
    , periodLength
    , ptTypes
    , netInp
    , modelBuilder
    , actionConfig
    , expSetup
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function                   (on)
import           Data.List                       (find, foldl', genericLength, groupBy,
                                                  nub, sort, sortBy)
import qualified Data.Map                        as M
import           Data.Serialize                  as S
import qualified Data.Text                       as T
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Directory
import           System.IO.Unsafe                (unsafePerformIO)
import           System.Random.MWC
import           Text.Printf

-- ANN modules
import           Grenade
import qualified TensorFlow.Core                 as TF hiding (value)
import qualified TensorFlow.GenOps.Core          as TF (relu', tanh')
import qualified TensorFlow.Minimize             as TF
import qualified TensorFlow.Session              as TF

import           Experimenter                    hiding (sum)
import           ML.BORL                         as B
import           SimSim

import           Releaser.Action
import           Releaser.ActionFilter
import           Releaser.Costs
import           Releaser.Demand
import           Releaser.ReleasePLT
import           Releaser.Type


import           Debug.Trace

periodLength :: Time
periodLength = 1


buildSim :: IO SimSim
buildSim =
  newSimSimIO
    routing
    -- procTimesConst
    procTimes
    periodLength
           -- releaseImmediate
    (mkReleasePLT initialPLTS)
    dispatchFirstComeFirstServe
    shipOnDueDate

initialPLTS :: M.Map ProductType Time
initialPLTS = M.fromList $ zip ptTypes [1 ..]

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))
                        ,(Product 2, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))])
            ,(Machine 2,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130/960) (170/960)))])
            ,(Machine 3,[(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180/960) (200/960)))])
            ]

procTimesConst :: ProcTimes
procTimesConst =
  [ (Machine 1, [(Product 1, return . const (timeFromDouble (100 / 960))), (Product 2, return . const (timeFromDouble (100 / 960)))])
  , (Machine 2, [(Product 1, return . const (timeFromDouble (150 / 960)))])
  , (Machine 3, [(Product 2, return . const (timeFromDouble (190 / 960)))])
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
  g <- createSystemRandom
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


instance Serialize Release where
  put (Release _ n) = S.put $ T.unpack n
  get = do
    n <- T.pack <$> S.get
    let fun | n == pltReleaseName = mkReleasePLT initialPLTS
            | n ==  uniqueReleaseName releaseImmediate = releaseImmediate
            | T.isPrefixOf bilName n = releaseBIL $ M.fromList bilArgs
              where ~bilArgs = read (T.unpack $ T.drop (T.length bilName) n)
                    ~bilName = T.takeWhile (/= '[') $ uniqueReleaseName (releaseBIL mempty)
    return fun


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
decay t p@(Parameters alp bet del ga eps exp rand zeta xi) =
  Parameters
    (max 0.03 $ decay slow * alp)
    (max 0.015 $ decay slow * bet)
    (max 0.015 $ decay slow * del)
    (max 0.015 $ decay slow * ga)
    (max 0.05 $ decay slow * eps)
    (max 0.05 $ decay slower * exp)
    rand
    zeta
    (0.5 * bet)
  where
    slower = 0.01
    slow = 0.005
    decaySteps = 150000 :: Double
    decay rate = rate ** (fromIntegral t / decaySteps)


-- SuperSimple: AggregatedOverProductTypes - OrderPool+Shipped

netInpPre :: Bool -> St -> [[Double]]
netInpPre useReduce (St sim _ _ plts) =
  [ map ((if useReduce then scaleValue (1, 7) else id) . timeToDouble) (M.elems plts)
  , map reduce $ mkFromList (simOrdersOrderPool sim) -- TODO: split also by product type
  , map reduce $ map genericLength (sortByTimeUntilDue (-actFilMaximumPLT actionFilterConfig) 0 currentTime (simOrdersShipped sim))
  ]
  where
    currentTime = simCurrentTime sim
    mkFromList xs = map genericLength (sortByTimeUntilDue (actFilMinimumPLT actionFilterConfig) (actFilMaximumPLT actionFilterConfig) currentTime xs)
    reduce x | useReduce = scaleValue (0, 12) x
             | otherwise = x

netInp :: St -> [Double]
netInp = concat . netInpPre True

netInpTbl :: St -> [Double]
netInpTbl st =
  case netInpPre False st of
    [plts, opOrds, shipOrds] -> plts ++ map reduce (opOrds ++ shipOrds)
  where
    reduce x = 7 * fromIntegral (ceiling (x / 7))


nnConfig :: NNConfig St
nnConfig =
  NNConfig
    { _toNetInp = netInp
    , _replayMemoryMaxSize = 40000
    , _trainBatchSize = 128
    , _grenadeLearningParams = LearningParameters 0.01 0.9 0.0001
    , _prettyPrintElems = ppSts
    , _scaleParameters = scalingByMaxAbsReward False 20
    , _updateTargetInterval = 10000
    , _trainMSEMax = Just 0.03
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
  show st = filter (/= '"') $ show $ map (map printFloat) $ netInpPre False st
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
  let initSt = St sim startOrds RewardPeriodEndSimple (M.fromList $ zip (productTypes sim) (map Time [1,1..]))
  let (actionList, actions) = mkConfig (actionsPLT initSt) actionConfig
  let actionFilter = mkConfig (actionFilterPLT actionList) actionFilterConfig
  let initVals = InitValues 0 0 0 0 0
  let alg = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
  return $ mkUnichainTabular alg initSt netInpTbl actions actionFilter borlParams decay (Just initVals)


buildBORLTensorflow :: (MonadBorl' m) => m (BORL St)
buildBORLTensorflow = do
  sim <- liftSimple buildSim
  startOrds <- liftSimple $ generateOrders sim
  let initSt = St sim startOrds RewardPeriodEndSimple (M.fromList $ zip (productTypes sim) (map Time [1,1..]))
  let (actionList, actions) = mkConfig (actionsPLT initSt) actionConfig
  let actionFilter = mkConfig (actionFilterPLT actionList) actionFilterConfig
  let alg = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
  let initVals = InitValues 25 0 0 0 0
  mkUnichainTensorflowM alg initSt actions actionFilter borlParams decay (modelBuilder actions initSt) nnConfig (Just initVals)

------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------

instance ExperimentDef (BORL St) where

  type ExpM (BORL St) = TF.SessionT IO
  -- type ExpM (BORL St) = IO

  type Serializable (BORL St) = BORLSerialisable StSerialisable
  serialisable = toSerialisableWith serializeSt
  deserialisable ser =
    unsafePerformIO $
    runMonadBorlTF $ do
      borl <- liftTensorflow buildBORLTensorflow
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
          ser
  type InputValue (BORL St) = [Order]
  type InputState (BORL St) = ()


  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput _ borl _ _ = do
    let (St _ inc _ _) = borl ^. s
    return (inc, ())


  -- ^ Run a step of the environment and return new state and result.
  -- runStep :: (MonadBorl' m) => a -> InputValue a -> E.Period -> m ([StepResult], a)
  runStep borl incOrds _ = do
    borl' <- stepM (set (s.nextIncomingOrders) incOrds borl)
    -- liftSimple $ putStrLn $ "Here: " <> show (borl ^. t)
    -- helpers
    when (borl ^. t `mod` 10000 == 0) $ liftSimple $ prettyBORLHead True borl >>= print

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
        avgRewMin = StepResult "MinAvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies.rhoMinimum.proxyScalar)
        pltP1 = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s.plannedLeadTimes))
        pltP2 = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s.plannedLeadTimes))
        psiRho = StepResult "PsiRho" (Just $ fromIntegral borlT) (borl' ^. psis._1)
        psiV = StepResult "PsiV" (Just $ fromIntegral borlT) (borl' ^. psis._2)
        psiW = StepResult "PsiW" (Just $ fromIntegral borlT) (borl' ^. psis._3)
        vAvg = StepResult "VAvg" (Just $ fromIntegral borlT) (avg $ borl' ^. lastRewards)
        reward = StepResult "Reward" (Just $ fromIntegral borlT) (head $ borl' ^. lastRewards)
        avgReward = StepResult "Reward" (Just $ fromIntegral borlT) (head $ borl' ^. lastRewards)
        avg xs = sum xs / fromIntegral (length xs)


    return ([-- cost related measures
              cSum, cEarn, cBoc, cWip, cFgi
             -- floor
            , curOp, curWip, curBo, curFgi, demand
             -- time related measures
            , tFtMeanFloorAndFgi, tFtStdDevFloorAndFgi, tTardPctFloorAndFgi, tTardMeanFloorAndFgi, tTardStdDevFloorAndFgi
            , tFtMeanFloor, tFtStdDevFloor, tTardPctFloor, tTardMeanFloor, tTardStdDevFloor
             -- BORL related measures
            , avgRew, avgRewMin, pltP1, pltP2, psiRho, psiV, psiW, vAvg, reward, avgReward
            ], borl')


  -- ^ Provides the parameter setting.
  -- parameters :: a -> [ParameterSetup a]
  parameters _ = [ ParameterSetup "Algorithm" (set algorithm) (view algorithm) (Just $ return . const [ algBORLNoScale
                                                                                                      , algVPsi
                                                                                                      , algDQN
                                                                                                      ]) Nothing Nothing Nothing
                   -- ParameterSetup "RewardType" (set (s.rewardFunctionOrders)) (view (s.rewardFunctionOrders)) (Just $ return . const [-- RewardShippedSimple,
                   --                                                                                                RewardPeriodEndSimple
                   --                                                                                                                   ]) Nothing Nothing Nothing
                 , ParameterSetup "ReleaseAlgorithm" (\r -> over (s.simulation) (\sim -> sim { simRelease = r })) (simRelease . view (s.simulation))
                   (Just $ return . const [ mkReleasePLT initialPLTS
                                          , releaseImmediate
                                          , releaseBIL (M.fromList [(Product 1, 5), (Product 2, 5)])
                                          , releaseBIL (M.fromList [(Product 1, 4), (Product 2, 4)])
                                          , releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])
                                          , releaseBIL (M.fromList [(Product 1, 2), (Product 2, 2)])
                                          ])
                 Nothing
                 (Just (\x -> uniqueReleaseName x /= pltReleaseName)) -- drop preparation phase for all release algorithms but the BORL releaser
                 (Just (\x -> if uniqueReleaseName x == pltReleaseName then FullFactory else SingleInstance)) -- only evaluate once if ImRe or BIL

                 ]
    where algVPsi = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
          algBORLNoScale = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal False


  -- ^ This function defines how to find experiments that can be resumed. Note that the experiments name is always a
  -- comparison factor, that is, experiments with different names are unequal.
  equalExperiments (borl1, _) (borl2, _) =
    (-- borl1^.s.nextIncomingOrders,
    borl1^. s.rewardFunctionOrders,
    borl1 ^. s.plannedLeadTimes,
    borl1 ^. t,
    borl1 ^. episodeNrStart,
    borl1 ^. B.parameters,
    borl1 ^. algorithm,
    borl1 ^. phase,
    borl1 ^. lastVValues,
    borl1 ^. lastRewards,
    borl1 ^. psis) ==
    (-- borl2^.s.nextIncomingOrders,
    borl2^. s.rewardFunctionOrders,
    borl2 ^. s.plannedLeadTimes,
    borl2 ^. t,
    borl2 ^. episodeNrStart,
    borl2 ^. B.parameters,
    borl2 ^. algorithm,
    borl2 ^. phase,
    borl2 ^. lastVValues,
    borl2 ^. lastRewards,
    borl2 ^. psis)

  -- HOOKS
  beforePreparationHook _ _ g borl = liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) borl

  beforeWarmUpHook _ _ _ g borl =
    liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $
      set (B.parameters . exploration) 0 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
      set (B.parameters . gamma) 0 $ set (B.parameters . zeta) 0 $ set (B.parameters . xi) 0 borl

  beforeEvaluationHook _ _ _ g borl = -- in case warm up phase is 0 periods
    liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $
      set (B.parameters . exploration) 0 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
      set (B.parameters . gamma) 0 $ set (B.parameters . zeta) 0 $ set (B.parameters . xi) 0 borl

  afterPreparationHook _ expNr repetNr = liftIO $ copyFiles "prep_" expNr repetNr Nothing
  afterWarmUpHook _ expNr repetNr repliNr = liftIO $ copyFiles "warmup_" expNr repetNr (Just repliNr)
  afterEvaluationHook _ expNr repetNr repliNr = liftIO $ copyFiles "eval_" expNr repetNr (Just repliNr)

copyFiles :: String -> ExperimentNumber -> RepetitionNumber -> Maybe ReplicationNumber -> IO ()
copyFiles pre expNr repetNr mRepliNr = do
  let dir = "results/" <> T.unpack (T.replace " " "_" $ expSetup ^. experimentBaseName) <> "/data/"
  createDirectoryIfMissing True dir
  mapM_ (\fn -> copyIfFileExists fn (dir <> pre <> fn <> "_exp_" <> show expNr <> "_rep_" <> show repetNr <> maybe "" (\x -> "_repl_" <> show x) mRepliNr)) ["reward", "stateValues"]


copyIfFileExists :: FilePath -> FilePath -> IO ()
copyIfFileExists fn target = do
  exists <- doesFileExist fn
  when exists $ copyFileWithMetadata fn target


expSetup :: ExperimentSetup
expSetup = ExperimentSetup
  { _experimentBaseName         = "ANN AggregatedOverProductTypes OrderPool+Shipped w. exp procTimes, unif demand"
  , _experimentRepetitions      =  1
  , _preparationSteps           =  300000
  , _evaluationWarmUpSteps      =  1000
  , _evaluationSteps            =  5000
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  1
  }
