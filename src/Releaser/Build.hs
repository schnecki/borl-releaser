{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}


module Releaser.Build
    ( buildBORLTable
    , buildBORLTensorflow
    , buildSim
    , nnConfig
    , netInp
    , modelBuilder
    , actionConfig
    , experimentName
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function                     (on)
import           Data.List                         (find, foldl', genericLength, groupBy,
                                                    nub, sort, sortBy)
import qualified Data.Map                          as M
import           Data.Serialize                    as S
import qualified Data.Text                         as T
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Directory
import           System.IO.Unsafe                  (unsafePerformIO)
import           System.Random.MWC
import           Text.Printf

-- ANN modules
import           Grenade
import qualified TensorFlow.Core                   as TF hiding (value)
import qualified TensorFlow.GenOps.Core            as TF (relu', tanh')
import qualified TensorFlow.Minimize               as TF
import qualified TensorFlow.Session                as TF

import           Experimenter                      hiding (sum)
import           ML.BORL                           as B hiding (actionFilter,
                                                         featureExtractor)
import qualified ML.BORL                           as B
import           SimSim                            hiding (productTypes)

import           Releaser.ActionFilter.Type
import           Releaser.Costs.Type
import           Releaser.Decay.Type
import           Releaser.FeatureExtractor.Type
import           Releaser.Release.ReleasePlt
import           Releaser.Reward
import           Releaser.Reward.Type
import           Releaser.Routing.Type
import           Releaser.SettingsAction
import           Releaser.SettingsActionFilter
import           Releaser.SettingsCosts
import           Releaser.SettingsDecay
import           Releaser.SettingsDemand
import           Releaser.SettingsFeatureExtractor
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type
import           Releaser.Util


import           Debug.Trace


buildSim :: IO SimSim
buildSim =
  newSimSimIO
    (configRoutingRoutes routing)
    -- procTimesConst
    procTimes
    periodLength
           -- releaseImmediate
    (mkReleasePLT initialPLTS)
    dispatchFirstComeFirstServe
    shipOnDueDate

initialPLTS :: M.Map ProductType Time
initialPLTS = M.fromList $ zip productTypes [1 ..]

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


-- testDemand :: IO ()
-- testDemand = do
--   let nr = 1000
--   g <- createSystemRandom
--   sim <- buildSim
--   xs <- replicateM nr (generateOrders sim)
--   let len = fromIntegral $ length (concat xs)
--   putStr "Avg order slack time: "
--   print $ timeToDouble (sum $ map orderSlackTime (concat xs)) / len
--   putStr "Avg order arrival date: "
--   print $ timeToDouble (sum $ map arrivalDate (concat xs)) / len
--   putStr "Avg number of order per period: "
--   print $ fromIntegral (sum (map length xs)) / fromIntegral nr
--   putStr "Avg order due date: "
--   print $ timeToDouble (sum $ map dueDate (concat xs)) / len
--   putStr "Avg number of order per product type"
--   print $ map length $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)
--   print $ map (productType . head) $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)

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


-- | BORL Parameters.
borlParams :: Parameters
borlParams = Parameters
  { _alpha            = 0.5
  , _beta             = 0.15
  , _delta            = 0.14
  , _gamma            = 0.90
  , _epsilon          = 0.5
  , _exploration      = 1.0
  , _learnRandomAbove = 0.0
  , _zeta             = 1.0
  , _xi               = 0.2
  }


instance Show St where
  show st = show (extractFeatures False st)


netInp :: St -> [Double]
netInp = extractionToList . extractFeatures True

netInpTbl :: St -> [Double]
netInpTbl st = case extractFeatures False st of
  Extraction plts op que fgi shipped -> plts ++ map reduce (concat $ op ++ concat que ++ fgi ++ shipped)
  where
    reduce x = 7 * fromIntegral (ceiling (x / 7))


nnConfig :: NNConfig
nnConfig =
  NNConfig
    { _replayMemoryMaxSize = 30000
    , _trainBatchSize = 128
    , _grenadeLearningParams = LearningParameters 0.01 0.9 0.0001
    , _prettyPrintElems = []    -- is set just before printing
    , _scaleParameters = scalingByMaxAbsReward False 50
    , _updateTargetInterval = 10000
    , _trainMSEMax = Nothing -- Just 0.03
    }

modelBuilder :: (TF.MonadBuild m) => [Action a] -> St -> m TensorflowModel
modelBuilder actions initState =
  buildModel $
  inputLayer1D len >>
  fullyConnected1D (3 * len) TF.relu' >>
  fullyConnected1D (2 * len) TF.relu' >>
  fullyConnected1D (ceiling (0.7 * fromIntegral len)) TF.relu' >>
  fullyConnected1D (ceiling (0.3 * fromIntegral len)) TF.relu' >>
  fullyConnected1D (genericLength actions) TF.tanh' >>
  trainingByAdam1DWith TF.AdamConfig {TF.adamLearningRate = 0.001, TF.adamBeta1 = 0.9, TF.adamBeta2 = 0.999, TF.adamEpsilon = 1e-8}
  where
    len = genericLength (netInp initState)


buildBORLTable :: IO (BORL St)
buildBORLTable = do
  sim <- buildSim
  startOrds <- generateOrders sim
  let initSt =
        St
          sim
          startOrds
           (RewardInFuture configRewardOpOrds ByOrderPoolOrders)
          --  (RewardPeriodEndSimple configRewardOrder)
          (M.fromList $ zip productTypes (map Time [1,1 ..]))
  let (actionList, actions) = mkConfig (action initSt) actionConfig
  let actFilter = mkConfig (actionFilter actionList) actionFilterConfig
  let alg = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
  return $ mkUnichainTabular alg initSt netInpTbl actions actFilter borlParams (configDecay decay) (Just initVals)

initVals :: InitValues
initVals = InitValues 0 0 0 0 0

buildBORLTensorflow :: (MonadBorl' m) => m (BORL St)
buildBORLTensorflow = do
  sim <- liftSimple buildSim
  startOrds <- liftSimple $ generateOrders sim
  let initSt = St sim startOrds (RewardPeriodEndSimple configRewardOrder) (M.fromList $ zip productTypes (map Time [1,1 ..]))
  let (actionList, actions) = mkConfig (action initSt) actionConfig
  let actFilter = mkConfig (actionFilter actionList) actionFilterConfig
  let alg = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
  mkUnichainTensorflowM alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilder actions initSt) nnConfig (Just initVals)

copyFiles :: String -> ExperimentNumber -> RepetitionNumber -> Maybe ReplicationNumber -> IO ()
copyFiles pre expNr repetNr mRepliNr = do
  let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
  createDirectoryIfMissing True dir
  mapM_ (\fn -> copyIfFileExists fn (dir <> pre <> fn <> "_exp_" <> show expNr <> "_rep_" <> show repetNr <> maybe "" (\x -> "_repl_" <> show x) mRepliNr)) ["reward", "stateValues"]

copyIfFileExists :: FilePath -> FilePath -> IO ()
copyIfFileExists fn target = do
  exists <- doesFileExist fn
  when exists $ copyFileWithMetadata fn target


------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------


instance ExperimentDef (BORL St) where

  type ExpM (BORL St) = TF.SessionT IO
  -- type ExpM (BORL St) = IO


  type Serializable (BORL St) = BORLSerialisable StSerialisable
  serialisable = toSerialisableWith serializeSt id
  deserialisable ser =
    unsafePerformIO $ runMonadBorlTF $ do
      borl <- liftTensorflow buildBORLTensorflow
      let (St sim _ _ _) = borl ^. s
      let (_, actions) = mkConfig (action (borl ^. s)) actionConfig
      return $
        fromSerialisableWith
          (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
          id
          actions
          (borl ^. B.actionFilter)
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
    borl' <- stepM (set (s . nextIncomingOrders) incOrds borl)
    -- helpers
    when (borl ^. t `mod` 10000 == 0) $ liftSimple $ prettyBORLHead True borl >>= print
    let simT = timeToDouble $ simCurrentTime $ borl' ^. s . simulation
    let borlT = borl' ^. t
    -- demand
    let demand = StepResult "demand" (Just simT) (fromIntegral $ length $ borl ^. s . nextIncomingOrders)
    -- cost related measures
    let (StatsOrderCost earnOld wipOld boOld fgiOld) = simStatsOrderCosts $ simStatistics (borl ^. s . simulation)
    let (StatsOrderCost earn wip bo fgi) = simStatsOrderCosts $ simStatistics (borl' ^. s . simulation)
    let cEarn = StepResult "EARN" (Just simT) (fromIntegral (earn - earnOld))
    let cBoc = StepResult "BOC" (Just simT) (boCosts costConfig * fromIntegral (bo - boOld))
    let cWip = StepResult "WIPC" (Just simT) (wipCosts costConfig * fromIntegral (wip - wipOld))
    let cFgi = StepResult "FGIC" (Just simT) (fgiCosts costConfig * fromIntegral (fgi - fgiOld))
    let cSum = StepResult "SUMC" (Just simT) (cBoc ^. resultYValue + cWip ^. resultYValue + cFgi ^. resultYValue)
    let curOp = StepResult "op" (Just simT) (fromIntegral $ length $ simOrdersOrderPool $ borl' ^. s . simulation)
    let curWip = StepResult "wip" (Just simT) (fromIntegral $ wip - wipOld)
    let curBo = StepResult "bo" (Just simT) (fromIntegral $ bo - boOld)
    let curFgi = StepResult "fgi" (Just simT) (fromIntegral $ fgi - fgiOld)
    -- time related measures
    let (StatsFlowTime ftNrFloorAndFgi (StatsOrderTime sumTimeFloorAndFgi stdDevFloorAndFgi _) mTardFloorAndFgi) = simStatsShopFloorAndFgi $ simStatistics (borl' ^. s . simulation)
    let tFtMeanFloorAndFgi = StepResult "FTMeanFloorAndFgi" (Just simT) (fromRational sumTimeFloorAndFgi / fromIntegral ftNrFloorAndFgi)
    let tFtStdDevFloorAndFgi = StepResult "FTStdDevFloorAndFgi" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloorAndFgi)
    let tTardPctFloorAndFgi = StepResult "TARDPctFloorAndFgi" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloorAndFgi) mTardFloorAndFgi)
    let tTardMeanFloorAndFgi = StepResult "TARDMeanFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloorAndFgi)
    let tTardStdDevFloorAndFgi = StepResult "TARDStdDevFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloorAndFgi)
    let (StatsFlowTime ftNrFloor (StatsOrderTime sumTimeFloor stdDevFloor _) mTardFloor) = simStatsShopFloor $ simStatistics (borl' ^. s . simulation)
    let tFtMeanFloor = StepResult "FTMeanFloor" (Just simT) (fromRational sumTimeFloor / fromIntegral ftNrFloor)
    let tFtStdDevFloor = StepResult "FTStdDevFloor" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloor)
    let tTardPctFloor = StepResult "TARDPctFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloor) mTardFloor)
    let tTardMeanFloor = StepResult "TARDMeanFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloor)
    let tTardStdDevFloor =
          StepResult "TARDStdDevFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloor)
    -- BORL' related measures
    let avgRew = StepResult "AvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies . rho . proxyScalar)
        avgRewMin = StepResult "MinAvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies . rhoMinimum . proxyScalar)
        pltP1 = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s . plannedLeadTimes))
        pltP2 = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s . plannedLeadTimes))
        psiRho = StepResult "PsiRho" (Just $ fromIntegral borlT) (borl' ^. psis . _1)
        psiV = StepResult "PsiV" (Just $ fromIntegral borlT) (borl' ^. psis . _2)
        psiW = StepResult "PsiW" (Just $ fromIntegral borlT) (borl' ^. psis . _3)
        vAvg = StepResult "VAvg" (Just $ fromIntegral borlT) (avg $ borl' ^. lastRewards)
        reward = StepResult "Reward" (Just $ fromIntegral borlT) (head $ borl' ^. lastRewards)
        avgReward = StepResult "Reward" (Just $ fromIntegral borlT) (head $ borl' ^. lastRewards)
        avg xs = sum xs / fromIntegral (length xs)
    return -- cost related measures
      ( [ cSum , cEarn , cBoc , cWip , cFgi
             -- floor
        , curOp , curWip , curBo , curFgi , demand
             -- time related measures
        , tFtMeanFloorAndFgi , tFtStdDevFloorAndFgi , tTardPctFloorAndFgi , tTardMeanFloorAndFgi , tTardStdDevFloorAndFgi , tFtMeanFloor , tFtStdDevFloor , tTardPctFloor , tTardMeanFloor , tTardStdDevFloor
             -- BORL related measures
        , avgRew , avgRewMin , pltP1 , pltP2 , psiRho , psiV , psiW , vAvg , reward , avgReward
        ]
      , borl')


  -- ^ Provides the parameter setting.
  -- parameters :: a -> [ParameterSetup a]
  parameters borl =
    [ ParameterSetup "Algorithm" (set algorithm) (view algorithm) (Just $ return . const [algBORLNoScale, algVPsi, algDQN]) Nothing Nothing Nothing
    , ParameterSetup "RewardType" (set (s . rewardFunctionOrders)) (view (s . rewardFunctionOrders)) (Just $ return . const [ RewardInFuture configRewardOpOrds ByOrderPoolOrders
                                                                                                                            -- , RewardPeriodEndSimple configRewardOrder
                                                                                                                            ]) Nothing Nothing Nothing
    , ParameterSetup
        "ReleaseAlgorithm"
        (\r -> over (s . simulation) (\sim -> sim {simRelease = r}))
        (simRelease . view (s . simulation))
        (Just $ return .
         const
           [ mkReleasePLT initialPLTS
           -- , releaseImmediate
           -- , releaseBIL (M.fromList [(Product 1, 6), (Product 2, 6)])
           -- , releaseBIL (M.fromList [(Product 1, 5), (Product 2, 5)])
           -- , releaseBIL (M.fromList [(Product 1, 4), (Product 2, 4)])
           -- , releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])
           -- , releaseBIL (M.fromList [(Product 1, 2), (Product 2, 2)])
           -- , releaseBIL (M.fromList [(Product 1, 1), (Product 2, 1)])
           ])
        Nothing
        (Just (\x -> uniqueReleaseName x /= pltReleaseName)) -- drop preparation phase for all release algorithms but the BORL releaser
        (Just
           (\x ->
              if uniqueReleaseName x == pltReleaseName
                then FullFactory
                else SingleInstance)) -- only evaluate once if ImRe or BIL
    ] ++
    [ParameterSetup "Training Batch Size" (setAllProxies  (proxyNNConfig.trainBatchSize)) (^?! proxies.v.proxyNNConfig.trainBatchSize) (Just $ return . const [128]) Nothing Nothing Nothing
    | isNN
    ]


    where
      algVPsi = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal True
      algBORLNoScale = AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 100) Normal False
      isNN = isNeuralNetwork (borl ^. proxies . v)

  -- HOOKS
  beforePreparationHook _ _ g borl =
    liftSimple $ do
      let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
      createDirectoryIfMissing True dir
      writeFile (dir ++ "plot.sh") gnuplot
      mapMOf (s . simulation) (setSimulationRandomGen g) borl
  beforeWarmUpHook expNr repetNr repliNr g borl =
    liftSimple $ do
      when (repliNr == 1) $ copyFiles "prep_" expNr repetNr Nothing -- afterPreparationHook seems not to be executed. Why? ***TODO***
      mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
        set (B.parameters . gamma) 0 $
        set (B.parameters . zeta) 0 $
        set (B.parameters . xi) 0 borl
  beforeEvaluationHook _ _ _ g borl -- in case warm up phase is 0 periods
   =
    liftSimple $ mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
    set (B.parameters . gamma) 0 $
    set (B.parameters . zeta) 0 $
    set (B.parameters . xi) 0 borl
  afterPreparationHook _ expNr repetNr = liftIO $ copyFiles "prep_" expNr repetNr Nothing
  afterWarmUpHook _ expNr repetNr repliNr = liftIO $ copyFiles "warmup_" expNr repetNr (Just repliNr)
  afterEvaluationHook _ expNr repetNr repliNr = liftIO $ copyFiles "eval_" expNr repetNr (Just repliNr)


experimentName :: T.Text
experimentName = "TEST FUTURE ANN AggregatedOverProductTypes OrderPool+Shipped w. exp procTimes, unif demand"

