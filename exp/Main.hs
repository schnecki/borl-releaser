{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where


import           Control.Lens
import           Network.HostName
import           Releaser.Build
import           Releaser.Type

import           Experimenter
import           ML.BORL           hiding (featureExtractor)

import           Releaser.Settings


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
    , _experimentInfoParameters = [actBounds, pltBounds, csts, dem, ftExtr, rout, dec, isNN, isTf] ++ concat [[replMem, batches, scaling, updateTarget] | isNNFlag]
    , _experimentRepetitions = 1
    , _preparationSteps = 300000
    , _evaluationWarmUpSteps = 1000
    , _evaluationSteps = 5000
    , _evaluationReplications = 3
    , _maximumParallelEvaluations = 1
    }
  where
    isNNFlag = isNeuralNetwork (borl ^. proxies . v)
    isNN = ExperimentInfoParameter "Is Neural Network" isNNFlag
    isTf = ExperimentInfoParameter "Is Tensorflow Network" (isTensorflow (borl ^. proxies . v))
    replMem = ExperimentInfoParameter "Replay Memory Size" (nnConfig ^. replayMemoryMaxSize)
    batches = ExperimentInfoParameter "Training Batch Size" (nnConfig ^. trainBatchSize)
    scaling = ExperimentInfoParameter "Scaling Setup" (nnConfig ^. scaleParameters)
    updateTarget = ExperimentInfoParameter "Target Network Update Interval" (nnConfig ^. updateTargetInterval)
    dec = ExperimentInfoParameter "Decay" (configDecayName decay)
    actBounds = ExperimentInfoParameter "Action Bounds" (configActLower actionConfig, configActUpper actionConfig)
    pltBounds = ExperimentInfoParameter "Action Filter (Min/Max PLT)" (configActFilterMin actionFilterConfig, configActFilterMax actionFilterConfig)
    csts = ExperimentInfoParameter "Costs" costConfig
    dem = ExperimentInfoParameter "Demand" (configDemandName demand)
    ftExtr = ExperimentInfoParameter "Feature Extractor (State Representation)" (configFeatureExtractorName $ featureExtractor True)
    rout = ExperimentInfoParameter "Routing (Simulation Setup)" (configRoutingName routing)


main :: IO ()
main = do

  run runMonadBorlIO runMonadBorlIO buildBORLTable   -- Lookup table version
  -- run runMonadBorlTF runMonadBorlTF buildBORLTensorflow -- ANN version

run :: (ExperimentDef a, a ~ BORL St, InputState a ~ ()) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> (ExpM a (Experiments a) -> IO (Experiments a)) -> ExpM a a -> IO ()
run runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  (changed, res) <- runExperimentsM runner dbSetting expSetting () mkInitSt
  liftSimple $ putStrLn $ "Any change: " ++ show changed
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
              -- , Id $ Last $ Of "TARDMeanFloorAndFGI", Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- , Id $ Last $ Of "TARDStdDevFloorAndFGI", Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- , Id $ Last $ Of "FTMeanFloor", Mean OverReplications (Last $ Of "FTMeanFloor")
              -- , Id $ Last $ Of "FTStdDevFloor", Mean OverReplications (Last $ Of "FTStdDevFloor")
              -- , Id $ Last $ Of "TARDPctFloor", Mean OverReplications (Last $ Of "TARDPctFloor")
              -- , Id $ Last $ Of "TARDMeanFloorAndFGI", Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- , Id $ Last $ Of "TARDStdDevFloorAndFGI", Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- , Id $ Last $ Of "AvgReward", Mean OverReplications (Last $ Of "AvgReward")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P1", Mean OverReplications (EveryXthElem 10 $ Of "PLT P1")
              -- , Id $ EveryXthElem 4 $ Of "PLT P1" -- , Mean OverReplications (EveryXthElem 1 $ Of "PLT P1")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P2", Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")
              -- , Id $ EveryXthElem 4 $ Of "PLT P2" -- , Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")

              , Id $ Last $ Of "PsiRho", Mean OverReplications (Last $ Of "PsiRho")
              , Id $ Last $ Of "PsiV", Mean OverReplications (Last $ Of "PsiV")
              , Id $ Last $ Of "PsiW", Mean OverReplications (Last $ Of "PsiW")
              ]
  evalRes <- genEvals runner2 dbSetting res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex evalRes

