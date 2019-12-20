{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where


import           Control.Lens
import           Control.Monad.IO.Class
import           Network.HostName
import           System.Environment (getArgs)

import           Experimenter
import           ML.BORL                hiding (featureExtractor)
import           TensorFlow.Session     hiding (run)

import           Releaser.Settings
import           Releaser.Build
import           Releaser.Type


databaseSetting :: IO DatabaseSetting
databaseSetting = do
  hostName <- getHostName
  return $ case hostName of
    "schnecki-zenbook" -> DatabaseSetting "host=schnecki-zenbook dbname=experimenter user=experimenter password=experimenter port=5432" 10
    _ -> DatabaseSetting "host=c437-pc141 dbname=experimenter user=experimenter password=experimenter port=5432" 10

expSetting :: BORL St -> ExperimentSetting
expSetting borl =
  ExperimentSetting
    { _experimentBaseName = experimentName
    , _experimentInfoParameters = [actBounds, pltBounds, csts, dem, ftExtr, rout, dec, isNN, isTf, pol] ++ concat [[updateTarget] | isNNFlag]
    , _experimentRepetitions = 1
    , _preparationSteps = 3000000
    , _evaluationWarmUpSteps = 1000
    , _evaluationSteps = 10000
    , _evaluationReplications = 30
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
    pol = ExperimentInfoParameter "Policy Exploration Strategy" (borl ^. ML.BORL.parameters . explorationStrategy)


main :: IO ()
main = do
  args <- getArgs
  hostName <- getHostName
  if hostName /= "c437-pc141" || any (== "run") args
    then
  -- run runMonadBorlIO runMonadBorlIO buildBORLTable   -- Lookup table version
    run runMonadBorlTF runMonadBorlTF buildBORLTensorflow -- ANN version

    else
  -- Generate results only
  -- loadAndEval runMonadBorlTF runMonadBorlTF buildBORLTensorflow -- ANN version

  -- Generate CSV only
    loadAndWriteCsv runMonadBorlTF runMonadBorlTF buildBORLTensorflow -- ANN version


run :: (ExperimentDef a, a ~ BORL St, InputState a ~ ()) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> (ExpM a (Experiments a) -> IO (Experiments a)) -> ExpM a a -> IO ()
run runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  (changed, res) <- runExperimentsM runner dbSetting expSetting () mkInitSt
  liftIO $ putStrLn $ "Any change: " ++ show changed
  eval dbSetting runner2 res

loadAndEval ::
     (SessionT IO (Maybe (Experiments (BORL St))) -> IO (Maybe (Experiments (BORL St))))
  -> (SessionT IO (Experiments (BORL St)) -> IO (Experiments (BORL St)))
  -> SessionT IO (BORL St)
  -> IO ()
loadAndEval runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  Just res <- loadExperimentsResultsM True runner dbSetting expSetting () mkInitSt 1
  eval dbSetting runner2 res

loadAndWriteCsv :: (SessionT IO (Maybe (Experiments (BORL St))) -> IO (Maybe (Experiments (BORL St)))) -> (SessionT IO () -> IO ()) -> SessionT IO (BORL St) -> IO ()
loadAndWriteCsv runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  Just res <- loadExperimentsResultsM False runner dbSetting expSetting () mkInitSt 1
  writeCsvMeasure runner2 dbSetting res (SmoothMovAvg 300) ["AvgReward"]


eval dbSetting runner2 res = do
  let evals = [ Sum OverPeriods $ Of "EARN", Mean OverReplications (Stats $ Sum OverPeriods $ Of "EARN")
              , Sum OverPeriods $ Of "BOC" , Mean OverReplications (Stats $ Sum OverPeriods $ Of "BOC")
              , Sum OverPeriods $ Of "WIPC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "WIPC")
              , Sum OverPeriods $ Of "FGIC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "FGIC")
              , Sum OverPeriods $ Of "SUMC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC")

              -- , Id $ EveryXthElem 4 $ Of "demand"
              -- , Id $ EveryXthElem 4 $ Of "op"
              -- , Id $ EveryXthElem 4 $ Of "wip"
              -- , Id $ EveryXthElem 4 $ Of "bo"
              -- , Id $ EveryXthElem 4 $ Of "fgi"

              -- , Id $ Last $ Of "FTMeanFloorAndFgi", Mean OverReplications (Last $ Of "FTMeanFloorAndFgi")
              -- , Id $ Last $ Of "FTStdDevFloorAndFgi", Mean OverReplications (Last $ Of "FTStdDevFloorAndFgi")
              -- , Id $ Last $ Of "TARDPctFloorAndFgi", Mean OverReplications (Last $ Of "TARDPctFloorAndFgi")
              -- , Id $ Last $ Of "TARDMeanFloorAndFGI"
              , Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- , Id $ Last $ Of "TARDStdDevFloorAndFGI"
              , Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- , Id $ Last $ Of "FTMeanFloor"
              , Mean OverReplications (Last $ Of "FTMeanFloor")
              -- , Id $ Last $ Of "FTStdDevFloor", Mean OverReplications (Last $ Of "FTStdDevFloor")
              -- , Id $ Last $ Of "TARDPctFloor", Mean OverReplications (Last $ Of "TARDPctFloor")
              -- , Id $ Last $ Of "TARDMeanFloor", Mean OverReplications (Last $ Of "TARDMeanFloor")
              -- , Id $ Last $ Of "TARDStdDevFloor", Mean OverReplications (Last $ Of "TARDStdDevFloor")

              -- , Id $ Last $ Of "AvgReward"
              , Mean OverReplications (Last $ Of "AvgReward")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P1", Mean OverReplications (EveryXthElem 10 $ Of "PLT P1")
              -- , Id $ EveryXthElem 4 $ Of "PLT P1" -- , Mean OverReplications (EveryXthElem 1 $ Of "PLT P1")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P2", Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")
              -- , Id $ EveryXthElem 4 $ Of "PLT P2" -- , Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")

              -- , Id $ Last $ Of "PsiRho", Mean OverReplications (Last $ Of "PsiRho")
              -- , Id $ Last $ Of "PsiV", Mean OverReplications (Last $ Of "PsiV")
              -- , Id $ Last $ Of "PsiW", Mean OverReplications (Last $ Of "PsiW")
              ]
  evalRes <- genEvals runner2 dbSetting res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex evalRes

