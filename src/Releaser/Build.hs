{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Releaser.Build
    ( buildBORLTable
    , buildBORLGrenade
    , buildBORLTensorflow
    , buildSim
    , nnConfig
    , netInp
    , modelBuilder
    , actionConfig
    , experimentName
    , mInverse
    , databaseSetting
    , expSetting
    , mkMiniPrettyPrintElems
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int                          (Int64)
import           Data.List                         (find, genericLength)
import           Data.Maybe                        (isJust)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as E

import           Control.Arrow                     (second)
import qualified Data.Map                          as M
import           Data.Serialize                    as S
import qualified Data.Text                         as T
import           Network.HostName
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Directory
import           System.Environment                (getArgs)
import           System.IO.Unsafe                  (unsafePerformIO)


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

import           Releaser.Costs.Type
import           Releaser.Decay.Type
import           Releaser.FeatureExtractor.Type
import           Releaser.Release.ReleasePlt
import           Releaser.Routing.Type
import           Releaser.SettingsAction
import           Releaser.SettingsActionFilter
import           Releaser.SettingsConfigParameters
import           Releaser.SettingsCosts
import           Releaser.SettingsDecay
import           Releaser.SettingsDemand
import           Releaser.SettingsFeatureExtractor
import           Releaser.SettingsPeriod
import           Releaser.SettingsReward
import           Releaser.SettingsRouting
import           Releaser.Type
import           Releaser.Util

import           Debug.Trace

buildSim :: IO SimSim
buildSim =
  newSimSimIO
    (configRoutingRoutes routing)
    procTimesConst
    -- procTimes
    periodLength
    -- releaseImmediate
    -- (releaseBIL $ M.fromList [(Product 1, 1), (Product 2, 1)])
    -- (releaseBIL $ M.fromList [(Product 1, 3), (Product 2, 3)])
    -- (releaseBIL $ M.fromList [(Product 1, 4), (Product 2, 4)])
    -- (releaseBIL $ M.fromList [(Product 1, 7), (Product 2, 7)])
    (mkReleasePLT initialPLTS)
    dispatchFirstComeFirstServe
    shipOnDueDate

initialPLTS :: M.Map ProductType Time
initialPLTS = M.fromList $ zip productTypes [1 ..]

procTimes :: ProcTimes
procTimes =
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
  [ (Machine 1, [ (Product 1, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))
                , (Product 2, fmap timeFromDouble . genContVar (uniformDistr (70 / 960) (130 / 960)))])
  , (Machine 2, [(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130 / 960) (170 / 960)))])
  , (Machine 3, [(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180 / 960) (200 / 960)))])
  ]

procTimesConst :: ProcTimes
procTimesConst =
  map (second $ filter ((`elem` productTypes) . fst)) $ filter ((`elem` allBlocks) . fst)
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


instance Show St where
  show st = show (extractFeatures False st)


netInp :: St -> [Double]
netInp = extractionToList . extractFeatures True

mInverse :: BORL St -> [Double] -> Maybe (Either String St)
mInverse borl = return . Left . show . fromListToExtraction (borl ^. s) (featureExtractor True)

netInpTbl :: St -> [Double]
netInpTbl st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> plts ++ map reduce (concat $ op ++ map (map (fromIntegral . ceiling . (/9))) (concat que) ++ fgi ++ shipped)
  where
    reduce x = 7 * fromIntegral (ceiling (x / 7))

netInpTblBinary :: St -> [Double]
netInpTblBinary st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> plts ++ map reduce (concat op) ++ map (fromIntegral . ceiling . (/9)) (concat (concat que)) ++ map reduce (concat $ fgi ++ shipped)
  where
    reduce x | x == 0 = x
             | otherwise = 1


modelBuilder :: (TF.MonadBuild m) => [Action a] -> St -> Int64 -> m TensorflowModel
modelBuilder actions initState cols =
  buildModel $
  inputLayer1D lenIn >>
  -- fullyConnected [10 * lenIn] TF.relu' >>
  fullyConnected [10 * fromIntegral lenIn] TF.relu' >>
  fullyConnected [5 * fromIntegral lenIn] TF.relu' >>
  fullyConnected [5 * fromIntegral lenIn] TF.relu' >>
  -- fullyConnected [lenActs, cols] TF.relu' >>
  -- fullyConnected [lenActs, cols] TF.relu' >>
  fullyConnected [lenActs, cols] TF.tanh' >>
  trainingByAdamWith TF.AdamConfig {TF.adamLearningRate = 0.01, TF.adamBeta1 = 0.9, TF.adamBeta2 = 0.999, TF.adamEpsilon = 1e-8}
  ----- trainingByGradientDescent 0.01
  where
    lenIn = genericLength (netInp initState)
    lenActs = genericLength actions
    lenOut = lenActs * cols


mkInitSt :: SimSim -> [Order] -> (St, [Action St], St -> [Bool])
mkInitSt sim startOrds =
  let initSt = St sim startOrds rewardFunction (M.fromList $ zip productTypes (repeat (Time 1)))
      (actionList, actions) = mkConfig (action initSt) actionConfig
      actFilter = mkConfig (actionFilter actionList) actionFilterConfig
  in (initSt, actions, actFilter)


buildBORLTable :: IO (BORL St)
buildBORLTable = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  return $ mkUnichainTabular alg initSt netInpTbl -- netInpTblBinary
    actions actFilter borlParams (configDecay decay) (Just initVals)


-- makeNN ::
--      forall nrH nrL layers shapes. ( KnownNat nrH , KnownNat nrL , Last shapes ~ 'D1 nrL , Head shapes ~ 'D1 nrH , NFData (Tapes layers shapes) , NFData (Network layers shapes) , Serialize (Network layers shapes) , Network layers shapes ~ NN nrH nrL)
--   => St
--   -> [Action St]
--   -> IO (Network layers shapes)
-- makeNN initSt actions =
--   case (someNatVal (genericLength (netInp initSt)), someNatVal (genericLength actions)) of
--     (Just (SomeNat (_ :: P.Proxy netIn)), Just (SomeNat (_ :: P.Proxy netOut))) ->
--       withDict (unsafeCoerce (Dict :: Dict (KnownNat netIn, KnownNat netOut))) $ do
--         randomNetworkInitWith UniformInit :: IO (NN netIn netOut)

type NN inp out
   = Network '[ FullyConnected inp 80, Relu, FullyConnected 80 60, Relu, FullyConnected 60 40, Relu, FullyConnected 40 20, Relu, FullyConnected 20 out, Tanh]
             '[ 'D1 inp, 'D1 80, 'D1 80, 'D1 60, 'D1 60, 'D1 40, 'D1 40, 'D1 20, 'D1 20, 'D1 out, 'D1 out]

buildBORLGrenade :: IO (BORL St)
buildBORLGrenade = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  nn <- randomNetworkInitWith UniformInit :: IO (NN 22 9)
  flipObjective . setPrettyPrintElems <$> mkUnichainGrenade alg initSt netInp actions actFilter borlParams (configDecay decay) nn nnConfig (Just initVals)


buildBORLTensorflow :: (MonadBorl' m) => m (BORL St)
buildBORLTensorflow = do
  sim <- liftIO buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  flipObjective . setPrettyPrintElems <$> mkUnichainTensorflowCombinedNetM alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilder actions initSt) nnConfig (Just initVals)
  -- setPrettyPrintElems <$> mkUnichainTensorflowM alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilder actions initSt) nnConfig (Just initVals)

setPrettyPrintElems :: BORL St -> BORL St
setPrettyPrintElems borl = setAllProxies (proxyNNConfig . prettyPrintElems) (ppElems borl) borl
  where ppElems borl = mkMiniPrettyPrintElems (borl ^. s)

copyFiles :: String -> ExperimentNumber -> RepetitionNumber -> Maybe ReplicationNumber -> IO ()
copyFiles pre expNr repetNr mRepliNr = do
  let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
  createDirectoryIfMissing True dir
  mapM_
    (\fn -> copyIfFileExists fn (dir <> pre <> fn <> "_exp_" <> show expNr <> "_rep_" <> show repetNr <> maybe "" (\x -> "_repl_" <> show x) mRepliNr))
    ["reward", "stateValues", "episodeLength", "plts", "costs", "stateVAllStates", "stateWAllStates", "statePsiVAllStates", "statePsiWAllStates"]

copyIfFileExists :: FilePath -> FilePath -> IO ()
copyIfFileExists fn target = do
  exists <- doesFileExist fn
  when exists $ copyFileWithMetadata fn target


databaseSetting :: IO DatabaseSetting
databaseSetting = do
  hostName <- getHostName
  args <- getArgs
  let mHostArg = case find ((== "--host=") . take 7) args of
                   x@Just{} -> drop 7 <$> x
                   Nothing  -> drop 3 <$> find ((== "-h=") . take 3) args
  let getPsqlHost h
        | isJust mHostArg = maybe "" (E.encodeUtf8 .T.pack) mHostArg
        | h `elem` ["schnecki-zenbook", "schnecki-laptop"] = "192.168.1.110"
        | otherwise = "c437-pc141"
  putStrLn $ "Using DB-Host: " <> show (getPsqlHost hostName)
  return $ DatabaseSetting ("host=" <> getPsqlHost hostName <> " dbname=experimenter user=experimenter password=experimenter port=5432") 10


mkMiniPrettyPrintElems :: St -> [[Double]]
mkMiniPrettyPrintElems st
  | length xs /= length base' = error $ "wrong length in mkMiniPrettyPrintElems: " ++
                                show (length xs) ++ " instead of " ++ show (length base') ++ ". E.g.: " ++ show (map (unscaleValue (Just (scaleOrderMin, scaleOrderMax))) base')
  | otherwise = zipWith (++) plts (replicate (length plts) (map (scaleValue (Just (scaleOrderMin, scaleOrderMax))) xs))
  where
    base' = drop (length productTypes) (netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleValue (Just (scalePltsMin, scalePltsMax)) . fromIntegral) [minVal, minVal + maxVal `div` 2]
    plts = return $ map (scaleValue (Just (scalePltsMin, scalePltsMax))) (take (length productTypes) [3, 5])
    xs :: [Double]
    xs = xsSimple2
    xsFullSmallPS = concat
      [ [ 3, 5]
      , concat [[ 0, 0, 0, 0, 0, 0, 5],[ 0, 0, 0, 0, 0, 0, 3]]
      , concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0]]]
      ,[]
      , concat [[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0]]
      , concat [[ 0, 0, 0],[ 0, 0, 0]]
      ]
    xsSimpleSingleMachine = concat [concat [[ 0, 0, 0, 0, 0, 4, 8]], concat[ concat [[ 0, 0, 0, 8, 5,13, 0, 0]]],[], concat [[ 3, 7, 6, 0, 0, 0]], concat [[ 0, 0, 0]]]
    xsSimple =              concat [[ 0, 0, 1,14,14, 9, 4],concat [[12]],concat [[ 1]],concat [[ 4]],[ 3],[ 6, 1, 0, 0, 0, 0],[ 0, 0, 1]]
    xsSimple2 =             concat [concat [[ 0, 0, 0, 4,14,14, 6]], concat [ concat [[21]]], concat [[ 1]], concat[[15, 2, 0, 0, 0, 0]], concat [[ 0, 0, 0]]]
    xsFull =
      concat
        [ [0, 0, 0, 0, 3, 5, 5]
        , [0, 0, 0, 0, 0, 0, 7]
        , concat [[0, 0, 9, 3, 0, 0, 0, 0], [0, 0, 0, 0, 3, 6, 4, 0]]
        , concat [[0, 2, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]]
        , concat [[0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 2, 0, 0, 0]]
        -- , [0, 0, 0, 0, 1, 0, 0, 0]
        -- , [1, 0, 0, 0, 0, 0, 0, 0]
        -- , [0, 0, 0, 1, 0, 0, 0, 0]
        , [0, 0, 0, 0, 0, 0]
        , [4, 2, 2, 0, 0, 0]
        , [0, 0, 0]
        , [0, 0, 0]
        ]


------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------


instance ExperimentDef (BORL St) where
  type ExpM (BORL St) = TF.SessionT IO
--  type ExpM (BORL St) = IO
  type Serializable (BORL St) = BORLSerialisable StSerialisable
  serialisable = do
    res <- toSerialisableWith serializeSt id
    return res
  deserialisable ser =
    unsafePerformIO $ runMonadBorlTF $ do
      borl <- buildBORLTensorflow
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
  runStep phase borl incOrds _ = do
    borl' <- stepM (set (s . nextIncomingOrders) incOrds borl)
    -- helpers
    when (borl ^. t `mod` 5000 == 0) $ liftIO $ prettyBORLHead True (Just $ mInverse borl) borl >>= print
    let simT = timeToDouble $ simCurrentTime $ borl' ^. s . simulation
    let borlT = borl' ^. t
    -- demand
    let demand = StepResult "demand" (Just simT) (fromIntegral $ length $ borl ^. s . nextIncomingOrders)
    -- cost related measures
    let (StatsOrderCost earnOld wipOld boOld fgiOld) = simStatsOrderCosts $ simStatistics (borl ^. s . simulation)
    let (StatsOrderCost earn wip bo fgi) = simStatsOrderCosts $ simStatistics (borl' ^. s . simulation)
    let cEarn  = StepResult "EARN" (Just simT) (fromIntegral (earn - earnOld))
    let cBoc   = StepResult "BOC" (Just simT) (boCosts costConfig * fromIntegral (bo - boOld))
    let cWip   = StepResult "WIPC" (Just simT) (wipCosts costConfig * fromIntegral (wip - wipOld))
    let cFgi   = StepResult "FGIC" (Just simT) (fgiCosts costConfig * fromIntegral (fgi - fgiOld))
    let cSum   = StepResult "SUMC" (Just simT) (cBoc ^. resultYValue + cWip ^. resultYValue + cFgi ^. resultYValue)
    let curOp  = StepResult "op" (Just simT) (fromIntegral $ length $ simOrdersOrderPool $ borl' ^. s . simulation)
    let curWip = StepResult "wip" (Just simT) (fromIntegral $ wip - wipOld)
    let curBo  = StepResult "bo" (Just simT) (fromIntegral $ bo - boOld)
    let curFgi = StepResult "fgi" (Just simT) (fromIntegral $ fgi - fgiOld)
    -- time related measures
    let (StatsFlowTime ftNrFloorAndFgi (StatsOrderTime sumTimeFloorAndFgi stdDevFloorAndFgi _) mTardFloorAndFgi) = simStatsShopFloorAndFgi $ simStatistics (borl' ^. s . simulation)
    let tFtMeanFloorAndFgi     = StepResult "FTMeanFloorAndFgi" (Just simT) (fromRational sumTimeFloorAndFgi / fromIntegral ftNrFloorAndFgi)
    let tFtStdDevFloorAndFgi   = StepResult "FTStdDevFloorAndFgi" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloorAndFgi)
    let tTardPctFloorAndFgi    = StepResult "TARDPctFloorAndFgi" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloorAndFgi) mTardFloorAndFgi)
    let tTardMeanFloorAndFgi   = StepResult "TARDMeanFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloorAndFgi)
    let tTardStdDevFloorAndFgi = StepResult "TARDStdDevFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloorAndFgi)
    let (StatsFlowTime ftNrFloor (StatsOrderTime sumTimeFloor stdDevFloor _) mTardFloor) = simStatsShopFloor $ simStatistics (borl' ^. s . simulation)
    let tFtMeanFloor     = StepResult "FTMeanFloor" (Just simT) (fromRational sumTimeFloor / fromIntegral ftNrFloor)
    let tFtStdDevFloor   = StepResult "FTStdDevFloor" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloor)
    let tTardPctFloor    = StepResult "TARDPctFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloor) mTardFloor)
    let tTardMeanFloor   = StepResult "TARDMeanFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloor)
    let tTardStdDevFloor = StepResult "TARDStdDevFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloor)
    -- BORL' related measures
    let avgRew    = StepResult "AvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies . rho . proxyScalar)
        avgRewMin = StepResult "MinAvgReward" (Just $ fromIntegral borlT) (borl' ^?! proxies . rhoMinimum . proxyScalar)
        pltP1     = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s . plannedLeadTimes))
        pltP2     = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s . plannedLeadTimes))
        psiRho    = StepResult "PsiRho" (Just $ fromIntegral borlT) (borl' ^. psis . _1)
        psiV      = StepResult "PsiV" (Just $ fromIntegral borlT) (borl' ^. psis . _2)
        psiW      = StepResult "PsiW" (Just $ fromIntegral borlT) (borl' ^. psis . _3)
        vAvg      = StepResult "VAvg" (Just $ fromIntegral borlT) (avg $ borl' ^. lastRewards)
        reward    = StepResult "Reward" (Just $ fromIntegral borlT) (headWithDefault 0 $ borl' ^. lastRewards)
        avg xs    = sum xs / fromIntegral (length xs)
        headWithDefault d []    = d
        headWithDefault _ (x:_) = x
    return $

      if phase /= EvaluationPhase
      then ([
          avgRew
        , avgRewMin
        -- , vAvg
        , reward

        ], borl')

      else
      ( [-- cost related measures
          cSum
        , cEarn
        , cBoc
        , cWip
        , cFgi
             -- floor
        , curOp
        , curWip
        , curBo
        , curFgi
        , demand
             -- time related measures
        , tFtMeanFloorAndFgi
        , tFtStdDevFloorAndFgi
        , tTardPctFloorAndFgi
        , tTardMeanFloorAndFgi
        , tTardStdDevFloorAndFgi
        , tFtMeanFloor
        , tFtStdDevFloor
        , tTardPctFloor
        , tTardMeanFloor
        , tTardStdDevFloor
             -- BORL related measures
        , avgRew
        , avgRewMin
        , pltP1
        , pltP2
        , psiRho
        , psiV
        , psiW
        , vAvg
        , reward
        ]
      , borl')
  -- ^ Provides the parameter setting.
  -- parameters :: a -> [ParameterSetup a]
  parameters borl =
    [ ParameterSetup
        "Algorithm"
        (set algorithm)
        (view algorithm)
        (Just $ return .
         const
           [ AlgBORL defaultGamma0 defaultGamma1 ByStateValues Nothing
           , AlgDQNAvgRewAdjusted (Just 0.01) 0.6 1.0 ByStateValues
           -- , AlgDQNAvgRewAdjusted 0.8 0.995 (ByStateValuesAndReward 1.0 (ExponentialDecay (Just 0.8) 0.99 100000))
           -- , AlgDQN 0.99
           -- , AlgDQN 0.8
           ])
        Nothing
        Nothing
        Nothing
    , ParameterSetup
        "RewardType"
        (set (s . rewardFunctionOrders))
        (view (s . rewardFunctionOrders))
        (Just $ return .
         const
           [ RewardPeriodEndSimple configReward500
             -- , RewardInFuture configRewardFutureOpOrds ByOrderPoolOrders
             -- , RewardPeriodEndSimple configRewardPeriodEnd
           ])
        Nothing
        Nothing
        Nothing
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
           , releaseBIL (M.fromList [(Product 1, 4), (Product 2, 4)])
           , releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])
           , releaseBIL (M.fromList [(Product 1, 2), (Product 2, 2)])
           -- , releaseBIL (M.fromList [(Product 1, 1), (Product 2, 1)])
           ])
        Nothing
        (Just (\x -> uniqueReleaseName x /= pltReleaseName)) -- drop preparation phase for all release algorithms but the BORL releaser
        (Just
           (\x ->
              if uniqueReleaseName x == pltReleaseName
                then FullFactory
                else SingleInstance -- only evaluate once if ImRe or BIL
            ))
    ] ++
    [ ParameterSetup
        "Learn Random Above until Exploration hits"
        (set (B.parameters . learnRandomAbove))
        (^. B.parameters . learnRandomAbove)
        (Just $ return . const [0.15])
        Nothing
        Nothing
        Nothing
    ] ++
    [ ParameterSetup
      "Xi (at period 0)"
      (set (B.parameters . xi))
      (^. B.parameters . xi)
      (Just $ return . const [5e-3])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Zeta (at period 0)"
      (set (B.parameters . zeta))
      (^. B.parameters . zeta)
      (Just $ return . const [0.10])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Epsilon (at period 0)"
      (set (B.parameters . epsilon))
      (^. B.parameters . epsilon)
      (Just $ return . const [0.5])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Replay Memory Size"
      (setAllProxies (proxyNNConfig . replayMemoryMaxSize))
      (^?! proxies . v . proxyNNConfig . replayMemoryMaxSize)
      (Just $ return . const [30000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Training Batch Size"
      (setAllProxies (proxyNNConfig . trainBatchSize))
      (^?! proxies . v . proxyNNConfig . trainBatchSize)
      (Just $ return . const [4])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
    "ANN Learning Rate Decay"
      (setAllProxies (proxyNNConfig . learningParamsDecay))
      (^?! proxies . v . proxyNNConfig . learningParamsDecay)
      (Just $ return . const [ExponentialDecay (Just 1e-4) 0.05 150000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Train MSE Max"
      (setAllProxies (proxyNNConfig . trainMSEMax))
      (^?! proxies . v . proxyNNConfig . trainMSEMax)
      (Just $ return . const [Nothing])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ScaleParameters"
      (setAllProxies (proxyNNConfig . scaleParameters))
      (^?! proxies . v . proxyNNConfig . scaleParameters)
      (Just $ return . const [ScalingNetOutParameters (-800) 800 (-5000) 5000 (-5000) 5000 (-5000) 5000])
      Nothing
      Nothing
      Nothing
    | isNN
    ]
    where
      isNN = isNeuralNetwork (borl ^. proxies . v)
  -- HOOKS
  beforePreparationHook _ _ g borl =
    liftIO $ do
      let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
      createDirectoryIfMissing True dir
      writeFile (dir ++ "plot.sh") gnuplot
      mapMOf (s . simulation) (setSimulationRandomGen g) borl
  beforeWarmUpHook _ _ _ g borl =
    liftIO $
      mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0.05 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
        set (B.parameters . disableAllLearning) True $
        set (B.parameters . gamma) 0 $
        set (B.parameters . zeta) 0 $
        set (B.parameters . xi) 0 borl
  beforeEvaluationHook _ _ _ g borl -- in case warm up phase is 0 periods
   =
    liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0.05 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
    set (B.parameters . disableAllLearning) True $
    set (B.parameters . gamma) 0 $
    set (B.parameters . zeta) 0 $
    set (B.parameters . xi) 0 borl
  afterPreparationHook _ expNr repetNr = liftIO $ copyFiles "prep_" expNr repetNr Nothing
  afterWarmUpHook _ expNr repetNr repliNr = liftIO $ copyFiles "warmup_" expNr repetNr (Just repliNr)
  afterEvaluationHook _ expNr repetNr repliNr = liftIO $ copyFiles "eval_" expNr repetNr (Just repliNr)


expSetting :: BORL St -> ExperimentSetting
expSetting borl =
  ExperimentSetting
    { _experimentBaseName = experimentName
    , _experimentInfoParameters = [actBounds, pltBounds, csts, dem, ftExtr, rout, dec, isNN, isTf, pol] ++ concat [[updateTarget, annxpSmth] | isNNFlag]
    , _experimentRepetitions = 1
    , _preparationSteps = 20*10^6
    , _evaluationWarmUpSteps = 1000
    , _evaluationSteps = 10000
    , _evaluationReplications = 1
    , _maximumParallelEvaluations = 1
    }
  where
    isNNFlag = isNeuralNetwork (borl ^. proxies . v)
    isNN = ExperimentInfoParameter "Is Neural Network" isNNFlag
    isTf = ExperimentInfoParameter "Is Tensorflow Network" (isTensorflow (borl ^. proxies . v))
    updateTarget = ExperimentInfoParameter "Target Network Update Interval" (nnConfig ^. updateTargetInterval)
    dec = ExperimentInfoParameter "Decay" (configDecayName decay)
    actBounds = ExperimentInfoParameter "Action Bounds" (configActLower actionConfig, configActUpper actionConfig)
    pltBounds = ExperimentInfoParameter "Action Filter (Min/Max PLT)" (configActFilterMin actionFilterConfig, configActFilterMax actionFilterConfig)
    csts = ExperimentInfoParameter "Costs" costConfig
    dem = ExperimentInfoParameter "Demand" (configDemandName demand)
    ftExtr = ExperimentInfoParameter "Feature Extractor (State Representation)" (configFeatureExtractorName $ featureExtractor True)
    rout = ExperimentInfoParameter "Routing (Simulation Setup)" (configRoutingName routing)
    pol = ExperimentInfoParameter "Policy Exploration Strategy" (borl ^. B.parameters . explorationStrategy)
    annxpSmth = ExperimentInfoParameter "Setting Exp Smooth to 1" (nnConfig ^. setExpSmoothParamsTo1)

