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
  let evals = [ Id (Of "avgRew")
              , Mean OverReplications (Of "avgRew"), StdDev OverReplications (Of "avgRew")
              , Mean OverReplications (Stats $ Mean OverPeriods (Of "avgRew"))
              , Mean OverReplications (Of "psiRho"), StdDev OverReplications (Of "psiRho")
              , Mean OverReplications (Of "psiV"), StdDev OverReplications (Of "psiV")
              , Mean OverReplications (Of "psiW"), StdDev OverReplications (Of "psiW")
              , Mean OverReplications (Of "avgEpisodeLength"), StdDev OverReplications (Of "avgEpisodeLength")
              ]
  evalRes <- genEvals res evals
  print (view evalsResults evalRes)
  writeAndCompileLatex evalRes


expSetup :: ExperimentSetup
expSetup = ExperimentSetup
  { _experimentBaseName         = "Releaser Test: AggregatedOverProductTypes - OrderPool+Shipped"
  , _experimentRepetitions      =  1
  , _preparationSteps           =  0
  , _evaluationWarmUpSteps      =  0
  , _evaluationSteps            =  18
  , _evaluationReplications     =  3
  , _maximumParallelEvaluations =  1
  }
