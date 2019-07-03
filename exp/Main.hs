{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.DeepSeq  (NFData, force)
import           Control.Lens
import           Control.Monad    (unless, when)
import           Data.Function    (on)
import           Data.List        (find, sortBy)
import qualified Data.Text        as T
import           Data.Time.Clock  (diffUTCTime, getCurrentTime)
import           System.IO        (hFlush, stdout)
import           Text.PrettyPrint

import           Experimenter
import           ML.BORL
import           SimSim

import           Releaser.Build
import           Releaser.Type


main :: IO ()
main = do
  rl <- buildBORLTable
  let databaseSetup = DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
  (changed, res) <- runExperiments runMonadBorl databaseSetup expSetup () rl
  putStrLn $ "Any change: " ++ show changed
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
  evalRes <- genEvals res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex evalRes


expSetup :: ExperimentSetup
expSetup = ExperimentSetup
  { _experimentBaseName         = "Table AggregatedOverProductTypes - OrderPool+Shipped"
  , _experimentRepetitions      =  1
  , _preparationSteps           =  300000
  , _evaluationWarmUpSteps      =  1000
  , _evaluationSteps            =  5000
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  1
  }
