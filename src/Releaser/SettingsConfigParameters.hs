{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
module Releaser.SettingsConfigParameters where

import qualified Data.Map  as M
import qualified Data.Text as T

-- ANN modules
import           Grenade

import           ML.BORL   as B hiding (actionFilter, featureExtractor)
import           SimSim


useHeuristicToFillReplMem :: Maybe Release
useHeuristicToFillReplMem = Just $ releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])


-- | BORL Parameters.
borlParams :: Parameters
borlParams = Parameters
  { _alpha              = 0.05
  , _alphaANN           = 1.0
  , _beta               = 0.01
  , _betaANN            = 1.0
  , _delta              = 0.005
  , _deltaANN           = 1.0
  , _gamma              = 0.01
  , _gammaANN           = 1.0
  , _epsilon            = 1.0
  , _exploration        = 1.0
  , _learnRandomAbove   = 0.1
  , _zeta               = 0.0
  , _xi                 = 0.0075
  , _disableAllLearning = False
  }

nnConfig :: NNConfig
nnConfig =
  NNConfig
    { _replayMemoryMaxSize = 30000
    , _trainBatchSize = 32
    , _grenadeLearningParams = LearningParameters 0.01 0.9 0.0001
    , _prettyPrintElems = []    -- is set just before printing
    , _scaleParameters = scalingByMaxAbsReward False 30
    , _updateTargetInterval = 5000
    , _trainMSEMax = Just 0.04
    }


alg :: Algorithm
alg = AlgBORLVOnly (ByMovAvg 1000)
  -- AlgBORL defaultGamma0 defaultGamma1 (ByMovAvg 4000) Normal True

initVals :: InitValues
initVals = InitValues 0 0 0 0 0

experimentName :: T.Text
experimentName = "26.7. Setting PLT w. exp procTimes, unif demand"
