{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
module Releaser.SettingsConfigParameters where

import qualified Data.Map  as M
import qualified Data.Text as T

-- ANN modules
import           Grenade

import           ML.BORL   as B hiding (actionFilter, featureExtractor)
import           SimSim

-- useHeuristicToFillReplMem :: Maybe Release
-- useHeuristicToFillReplMem = Just $ releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])


-- | BORL Parameters.
borlParams :: Parameters Double
borlParams = Parameters
  { _alpha              = 0.01
  , _alphaANN           = 1.0
  , _beta               = 0.03
  , _betaANN            = 1.0
  , _delta              = 0.01
  , _deltaANN           = 1.0
  , _gamma              = 0.01
  , _gammaANN           = 1.0
  , _epsilon            = 0.5
  , _exploration        = 1.0
  , _learnRandomAbove   = 0.1
  , _zeta               = 0.01
  , _xi                 = 0.01
  , _disableAllLearning = False
  }


nnConfig :: NNConfig
nnConfig =
  NNConfig
    { _replayMemoryMaxSize = 10000
    , _trainBatchSize = 8
    , _grenadeLearningParams = LearningParameters 0.01 0.0 0.0001
    , _learningParamsDecay = ExponentialDecay (Just 0) 0.05 100000
    , _prettyPrintElems = [] -- is set just before printing
    , _scaleParameters =
      ScalingNetOutParameters (-400) 400 (-5000) 5000 (-300) 300 (-300) 300
      -- scalingByMaxAbsReward False 3000
    , _stabilizationAdditionalRho = 0
    , _stabilizationAdditionalRhoDecay = ExponentialDecay Nothing 0.05 100000
    , _updateTargetInterval = 1
    , _trainMSEMax = Nothing -- Just 0.03
    , _setExpSmoothParamsTo1 = True
    }


-- nnConfig :: NNConfig
-- nnConfig =
--   NNConfig
--     { _replayMemoryMaxSize = 30000
--     , _trainBatchSize = 32
--     , _grenadeLearningParams = LearningParameters 0.01 0.9 0.0001
--     , _prettyPrintElems = []    -- is set just before printing
--     , _scaleParameters = scalingByMaxAbsReward False 60
--     , _updateTargetInterval = 10000
--     , _trainMSEMax = Nothing -- Just 0.04 -- this makes only sense when using the simple extractor
--     }


alg :: Algorithm s
alg =

  -- AlgDQNAvgRewardFree 0.75 0.995 ByStateValues
  AlgBORL defaultGamma0 defaultGamma1 ByStateValues False Nothing
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)

initVals :: InitValues
initVals = InitValues 0 0 0 0 0

experimentName :: T.Text
experimentName = "30.7. Adaptive BORL Order Releaser with exp procTimes, unif demand"
