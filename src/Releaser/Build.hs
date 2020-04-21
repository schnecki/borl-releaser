{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Unsafe              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Releaser.Build
    ( buildBORLTable
    , buildBORLGrenade
    , buildBORLTensorflow
    , buildSim
    , nnConfig
    , netInp
    , modelBuilderTf
    , modelBuilderGrenade
    , actionConfig
    , experimentName
    , mInverse
    , databaseSetting
    , expSetting
    , mkMiniPrettyPrintElems
    ) where

import           Control.Arrow                     (second)
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Constraint                   (Dict (..))
import           Data.Int                          (Int64)
import           Data.List                         (find, genericLength)
import qualified Data.Map                          as M
import           Data.Maybe                        (isJust)
import qualified Data.Proxy                        as Px
import           Data.Reflection                   (reifyNat)
import           Data.Serialize                    as S
import qualified Data.Text                         as T
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as E
import qualified Data.Vector.Storable              as V
import           GHC.Exts                          (IsList (..))
import           GHC.TypeLits                      (KnownNat)
import           Grenade
import           Network.HostName
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Directory
import           System.Environment                (getArgs)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Unsafe.Coerce                     (unsafeCoerce)


-- ANN modules
import           Grenade
import qualified HighLevelTensorflow               as TF

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
    -- procTimesConst
    procTimes
    periodLength
    -- releaseImmediate
    -- (releaseBIL $ M.fromList [(Product 1, 1), (Product 2, 1)])
    -- (releaseBIL $ M.fromList [(Product 1, 2), (Product 2, 2)])
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


netInp :: St -> V.Vector Float
netInp = extractionToList . extractFeatures True

mInverse :: BORL St -> NetInputWoAction -> Maybe (Either String St)
mInverse borl = return . Left . show . fromListToExtraction (borl ^. s) (featureExtractor True)

netInpTbl :: St -> V.Vector Float
netInpTbl st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> V.fromList $ plts ++ map reduce (concat $ op ++ map (map (fromIntegral . ceiling . (/9))) (concat que) ++ fgi ++ shipped)
  where
    reduce x = 7 * fromIntegral (ceiling (x / 7))

netInpTblBinary :: St -> [Float]
netInpTblBinary st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> plts ++ map reduce (concat op) ++ map (fromIntegral . ceiling . (/9)) (concat (concat que)) ++ map reduce (concat $ fgi ++ shipped)
  where
    reduce x | x == 0 = x
             | otherwise = 1


modelBuilderTf :: (TF.MonadBuild m) => [Action a] -> St -> Int64 -> m TF.TensorflowModel
modelBuilderTf actions initState cols =
  TF.buildModel $ -- (TF.BuildSetup 0.001) $
  TF.inputLayer1D lenIn >>
  TF.fullyConnected [10 * lenIn] TF.relu' >>
  TF.fullyConnected [10 * lenIn] TF.relu' >>
  TF.fullyConnected [5 * lenIn] TF.relu' >>
  TF.fullyConnected [5 * lenIn] TF.relu' >>
  TF.fullyConnected [lenActs, cols] TF.relu' >>
  TF.fullyConnected [lenActs, cols] TF.relu' >>
  TF.fullyConnectedLinear [lenActs, cols] >>
  -- TF.fullyConnected [lenActs, cols] TF.tanh' >>
  TF.trainingByAdamWith TF.AdamConfig {TF.adamLearningRate = 0.00025, TF.adamBeta1 = 0.9, TF.adamBeta2 = 0.999, TF.adamEpsilon = 1e-8}
  -- trainingByRmsPropWith TF.RmsPropConfig {TF.rmsPropLearningRate = 0.00025, TF.rmsPropRho = 0.5, TF.rmsPropMomentum = 0.95, TF.rmsPropEpsilon = 0.01}
  -- trainingByGradientDescent 0.001
  where
    lenIn = fromIntegral $ V.length (netInp initState)
    lenActs = genericLength actions

-- | The definition for a feed forward network using the dynamic module. Note the nested networks. This network clearly is over-engeneered for this example!
-- modelBuilderGrenade :: [Action a] -> St -> Integer -> IO SpecConcreteNetwork
-- modelBuilderGrenade actions initState cols =
--   buildModelWith HeEtAl $
--   specFullyConnected lenIn (10 * lenIn) |=> specRelu1D (10 * lenIn) |=> specDropout (10*lenIn) 0.90 Nothing |=>
--   specFullyConnected (10*lenIn) (10*lenIn) |=> specRelu1D (10*lenIn) |=>
--   specFullyConnected (10*lenIn) (5*lenOut) |=> specRelu1D (5*lenOut) |=>
--   specFullyConnected (5*lenOut) (2*lenOut) |=> specRelu1D (2*lenOut) |=>
--   specFullyConnected (2*lenOut) lenOut |=> specReshape (lenOut, 1, 1) (lenActs, cols, 1) |=> specTanh (lenActs, cols, 1) |=>
--   specNil (lenActs, cols, 1)
--   -- (if cols > 1
--   --   then specReshape1D2D lenOut (lenActs, cols) |=> specTanh2D (lenActs, cols) |=> specNil2D (lenActs, cols)
--   --   else specTanh1D lenActs |=> specNil1D lenOut)
--   where
--     lenOut = lenActs * cols
--     lenIn = fromIntegral $ V.length (netInp initState)
--     lenActs = genericLength actions
--     buildModelWith = networkFromSpecificationWith

modelBuilderGrenade :: [Action a] -> St -> Integer -> IO SpecConcreteNetwork
modelBuilderGrenade actions initState cols =
  buildModelWith HeEtAl BuildSetup { printResultingSpecification = False } $
  inputLayer1D lenIn >>
  fullyConnected (10*lenIn) >> relu >> dropout 0.90 >>
  fullyConnected (10*lenIn) >> relu >>
  fullyConnected (5*lenIn) >> relu >>
  fullyConnected (2*lenOut) >> relu >>
  fullyConnected lenOut >> reshape (lenActs, cols, 1) >> tanhLayer
  where
    lenOut = lenActs * cols
    lenIn = fromIntegral $ V.length (netInp initState)
    lenActs = genericLength actions


mkInitSt :: SimSim -> [Order] -> (AgentType -> IO St, [Action St], St -> V.Vector Bool)
mkInitSt sim startOrds =
  let initSt = St sim startOrds rewardFunction (M.fromList $ zip productTypes (repeat (Time 1)))
      (actionList, actions) = mkConfig (action initSt) actionConfig
      actFilter = mkConfig (actionFilter actionList) actionFilterConfig
  in (return . const initSt, actions, actFilter)


buildBORLTable :: IO (BORL St)
buildBORLTable = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  mkUnichainTabular alg initSt netInpTbl -- netInpTblBinary
    actions actFilter borlParams (configDecay decay) (Just initVals)

buildBORLGrenade :: IO (BORL St)
buildBORLGrenade = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  st <- liftIO $ initSt MainAgent
  flipObjective . setPrettyPrintElems <$> mkUnichainGrenade alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilderGrenade actions st) nnConfig (Just initVals)

buildBORLTensorflow :: (MonadBorl' m) => m (BORL St)
buildBORLTensorflow = do
  sim <- liftIO buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actions, actFilter) = mkInitSt sim startOrds
  st <- liftIO $ initSt MainAgent
  flipObjective . setPrettyPrintElems <$> mkUnichainTensorflowCombinedNetM alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilderTf actions st) nnConfig (Just initVals)
  -- setPrettyPrintElems <$> mkUnichainTensnorflowM alg initSt netInp actions actFilter borlParams (configDecay decay) (modelBuilderTf actions initSt) nnConfig (Just initVals)

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


mkMiniPrettyPrintElems :: St -> [V.Vector Float]
mkMiniPrettyPrintElems st
  | length (head xs) /= length base' = error $ "wrong length in mkMiniPrettyPrintElems: " ++
                                show (length $ head xs) ++ " instead of " ++ show (length base') ++ ". E.g.: " ++ show (map (unscaleValue (Just (scaleOrderMin, scaleOrderMax))) base') ++
                                "\nCurrent state: " ++ show (extractFeatures True st)

  | otherwise = map V.fromList $ concatMap (zipWith (++) plts . replicate (length plts) . map (scaleValue (Just (scaleOrderMin, scaleOrderMax)))) xs
  where
    base' = drop (length productTypes) (V.toList $ netInp st)
    plts :: [[Float]]
    plts = map (map (scaleValue (Just (scalePltsMin, scalePltsMax))) . take (length productTypes)) [[1, 3], [3, 5]]
    xs :: [[Float]]
    xs = [xsSimple, xsSimple2]
    xsSimple =              concat [concat [[ 0, 6, 8, 4, 4, 9, 9]], concat [ concat [[ 2]        ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 5, 4]]]
                            -- concat [concat [[ 0, 6, 8, 4, 4, 9, 9]], concat [ concat [[ 2],  [2],[2]  ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 5, 4]]]
    xsSimple2 =             concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[23]         ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 9, 0]]
                                   -- concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[23],  [2],[12]  ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 9, 0]]
                                   ]


------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------


instance ExperimentDef (BORL St) where
  -- type ExpM (BORL St) = TF.SessionT IO
  type ExpM (BORL St) = IO
  type Serializable (BORL St) = BORLSerialisable StSerialisable
  serialisable = do
    res <- toSerialisableWith serializeSt id
    return res
  deserialisable ser =
    -- runMonadBorlTF $ do
      -- borl <- buildBORLTensorflow
    unsafePerformIO $ do
      borl <- buildBORLGrenade
      let (St sim _ _ _) = borl ^. s
      let (_, actions) = mkConfig (action (borl ^. s)) actionConfig
      return $
        fromSerialisableWith
          (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
          id
          actions
          (borl ^. B.actionFilter)
          netInp
          (modelBuilderTf actions (borl ^. s))
          ser
  type InputValue (BORL St) = [Order]
  type InputState (BORL St) = ()
  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput !_ !borl !_ !_ = do
    let !(St !_ !inc !_ !_) = borl ^. s
    return (inc, ())
  -- ^ Run a step of the environment and return new state and result.
  -- runStep :: (MonadBorl' m) => a -> InputValue a -> E.Period -> m ([StepResult], a)
  runStep !phase !borl !incOrds !_ = do
    !borl' <- stepM (set (s . nextIncomingOrders) incOrds borl)
    -- helpers
    when (borl ^. t `mod` 5000 == 0) $ liftIO $ prettyBORLHead True (Just $ mInverse borl) borl >>= print
    let !simT = timeToDouble $ simCurrentTime $ borl' ^. s . simulation
    let !borlT = borl' ^. t
    -- demand
    let !demand = StepResult "demand" (Just simT) (fromIntegral $ length $ borl ^. s . nextIncomingOrders)
    -- cost related measures
    let (StatsOrderCost !earnOld !wipOld !boOld !fgiOld) = simStatsOrderCosts $ simStatistics (borl ^. s . simulation)
    let (StatsOrderCost !earn !wip !bo !fgi) = simStatsOrderCosts $ simStatistics (borl' ^. s . simulation)
    let !cEarn  = StepResult "EARN" (Just simT) (fromIntegral (earn - earnOld))
    let !cBoc   = StepResult "BOC" (Just simT) (boCosts costConfig * fromIntegral (bo - boOld))
    let !cWip   = StepResult "WIPC" (Just simT) (wipCosts costConfig * fromIntegral (wip - wipOld))
    let !cFgi   = StepResult "FGIC" (Just simT) (fgiCosts costConfig * fromIntegral (fgi - fgiOld))
    let !cSum   = StepResult "SUMC" (Just simT) (cBoc ^. resultYValue + cWip ^. resultYValue + cFgi ^. resultYValue)
    let !curOp  = StepResult "op" (Just simT) (fromIntegral $ length $ simOrdersOrderPool $ borl' ^. s . simulation)
    let !curWip = StepResult "wip" (Just simT) (fromIntegral $ wip - wipOld)
    let !curBo  = StepResult "bo" (Just simT) (fromIntegral $ bo - boOld)
    let !curFgi = StepResult "fgi" (Just simT) (fromIntegral $ fgi - fgiOld)
    -- time related measures
    let (StatsFlowTime !ftNrFloorAndFgi !(StatsOrderTime !sumTimeFloorAndFgi !stdDevFloorAndFgi !_) !mTardFloorAndFgi) = simStatsShopFloorAndFgi $ simStatistics (borl' ^. s . simulation)
    let !tFtMeanFloorAndFgi     = StepResult "FTMeanFloorAndFgi" (Just simT) (fromRational sumTimeFloorAndFgi / fromIntegral ftNrFloorAndFgi)
    let !tFtStdDevFloorAndFgi   = StepResult "FTStdDevFloorAndFgi" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloorAndFgi)
    let !tTardPctFloorAndFgi    = StepResult "TARDPctFloorAndFgi" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloorAndFgi) mTardFloorAndFgi)
    let !tTardMeanFloorAndFgi   = StepResult "TARDMeanFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloorAndFgi)
    let !tTardStdDevFloorAndFgi = StepResult "TARDStdDevFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloorAndFgi)
    let !(StatsFlowTime !ftNrFloor !(StatsOrderTime !sumTimeFloor !stdDevFloor !_) !mTardFloor) = simStatsShopFloor $ simStatistics (borl' ^. s . simulation)
    let !tFtMeanFloor     = StepResult "FTMeanFloor" (Just simT) (fromRational sumTimeFloor / fromIntegral ftNrFloor)
    let !tFtStdDevFloor   = StepResult "FTStdDevFloor" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloor)
    let !tTardPctFloor    = StepResult "TARDPctFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloor) mTardFloor)
    let !tTardMeanFloor   = StepResult "TARDMeanFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloor)
    let !tTardStdDevFloor = StepResult "TARDStdDevFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloor)
    -- BORL' related measures
    let val !l = realToFrac (borl' ^?! l)
    let !avgRew    = StepResult "AvgReward" (Just $ fromIntegral borlT) (val $ proxies . rho . proxyScalar)
        !avgRewMin = StepResult "MinAvgReward" (Just $ fromIntegral borlT) (val $ proxies . rhoMinimum . proxyScalar)
        !pltP1     = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s . plannedLeadTimes))
        !pltP2     = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s . plannedLeadTimes))
        !psiRho    = StepResult "PsiRho" (Just $ fromIntegral borlT) (val $ psis . _1)
        !psiV      = StepResult "PsiV" (Just $ fromIntegral borlT) (val $ psis . _2)
        !psiW      = StepResult "PsiW" (Just $ fromIntegral borlT) (val $ psis . _3)
        !vAvg      = StepResult "VAvg" (Just $ fromIntegral borlT) (avg $ borl' ^. lastRewards)
        !reward    = StepResult "Reward" (Just $ fromIntegral borlT) (realToFrac $headWithDefault 0 $ borl' ^. lastRewards)
        avg !xs    = realToFrac $ sum xs / fromIntegral (length xs)
        headWithDefault d []    = d
        headWithDefault _ (x:_) = x
    return $! force $
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
           [ AlgDQNAvgRewAdjusted 0.85 1.0 ByStateValues
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
           [ -- RewardPeriodEndSimple (ConfigRewardCosts (Just 750))
            RewardPeriodEndSimple (ConfigRewardCosts (Just 500))
           , RewardPeriodEndSimple (ConfigRewardCosts (Just 1000))
           -- , RewardPeriodEndSimple (ConfigRewardCosts Nothing)
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
    -- [ ParameterSetup
    --     "Learn Random Above until Exploration hits"
    --     (set (B.parameters . learnRandomAbove))
    --     (^. B.parameters . learnRandomAbove)
    --     (Just $ return . const [0.15])
    --     Nothing
    --     Nothing
    --     Nothing
    -- ] ++
    -- [ ParameterSetup
    --   "Xi (at period 0)"
    --   (set (B.parameters . xi))
    --   (^. B.parameters . xi)
    --   (Just $ return . const [5e-3])
    --   Nothing Nothing Nothing
    -- ] ++
    -- [ ParameterSetup
    --   "Zeta (at period 0)"
    --   (set (B.parameters . zeta))
    --   (^. B.parameters . zeta)
    --   (Just $ return . const [0.10])
    --   Nothing Nothing Nothing
    -- ] ++
    [ ParameterSetup
      "Alpha (at period 0)"
      (set (B.parameters . alpha))
      (^. B.parameters . alpha)
      (Just $ return . const [0.01])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Alpha"
      (set (B.decaySetting . alpha))
      (^. B.decaySetting . alpha)
      (Just $ return . const [ExponentialDecay Nothing 0.25 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Delta (at period 0)"
      (set (B.parameters . delta))
      (^. B.parameters . delta)
      (Just $ return . const [0.005])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Delta"
      (set (B.decaySetting . delta))
      (^. B.decaySetting . delta)
      (Just $ return . const [ExponentialDecay (Just 5e-4) 0.35 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Gamma (at period 0)"
      (set (B.parameters . gamma))
      (^. B.parameters . gamma)
      (Just $ return . const [0.01])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Gamma"
      (set (B.decaySetting . gamma))
      (^. B.decaySetting . gamma)
      (Just $ return . const [ExponentialDecay (Just 1e-3) 0.35 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Epsilon (at period 0)"
      (set (B.parameters . epsilon))
      (^. B.parameters . epsilon)
      (Just $ return . const [fromList [0.30, 0.01]])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Epsilon"
      (set (B.decaySetting . epsilon))
      (^. B.decaySetting . epsilon)
      (Just $ return . const [fromList [NoDecay]])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Exploration (at period 0)"
      (set (B.parameters . exploration))
      (^. B.parameters . exploration)
      (Just $ return . const [1.0])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Exploration"
      (set (B.decaySetting . exploration))
      (^. B.decaySetting . exploration)
      (Just $ return . const [ExponentialDecay Nothing 0.35 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Replay Memory Size"
      (setAllProxies (proxyNNConfig . replayMemoryMaxSize))
      (^?! proxies . v . proxyNNConfig . replayMemoryMaxSize)
      (Just $ return . const [20000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Replay Memory Strategy"
      (setAllProxies (proxyNNConfig . replayMemoryStrategy))
      (^?! proxies . v . proxyNNConfig . replayMemoryStrategy)
      (Just $ return . const [ReplayMemoryPerAction, ReplayMemorySingle])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Workers Min Exploration"
      (setAllProxies (proxyNNConfig . workersMinExploration))
      (^?! proxies . v . proxyNNConfig . workersMinExploration)
      (Just $ return . const [[0.5, 0.3, 0.15, 0.10, 0.05, 0.025, 0.01]])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Training Batch Size"
      (setAllProxies (proxyNNConfig . trainBatchSize))
      (^?! proxies . v . proxyNNConfig . trainBatchSize)
      (Just $ return . const [12])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ANN Learning Rate Decay"
      (setAllProxies (proxyNNConfig . learningParamsDecay))
      (^?! proxies . v . proxyNNConfig . learningParamsDecay)
      (Just $ return . const [ExponentialDecay Nothing 0.85 50000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ScaleParameters"
      (setAllProxies (proxyNNConfig . scaleParameters))
      (^?! proxies . v . proxyNNConfig . scaleParameters)
      (Just $ return . const [ScalingNetOutParameters (-800) 800 (-5000) 5000 (-3000) 3000 (-3000) 3000])
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
        set (B.settings . disableAllLearning) True $
        set (B.parameters . gamma) 0 $
        set (B.parameters . zeta) 0 $
        set (B.parameters . xi) 0 borl
  beforeEvaluationHook _ _ _ g borl -- in case warm up phase is 0 periods
   =
    liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0.05 $ set (B.parameters . alpha) 0 $ set (B.parameters . beta) 0 $
    set (B.settings . disableAllLearning) True $
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
    , _experimentInfoParameters = [actBounds, pltBounds, csts, dem, ftExtr, rout, dec, isNN, isTf, pol] ++ concat [[updateTarget] | isNNFlag]
    , _experimentRepetitions = 1
    , _preparationSteps = 1 * 10 ^ 6
    , _evaluationWarmUpSteps = 1000
    , _evaluationSteps = 100000
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
    pol = ExperimentInfoParameter "Policy Exploration Strategy" (borl ^. B.settings . explorationStrategy)
