{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
module Releaser.SettingsConfigParameters where

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Text    as T

-- ANN modules
import           Grenade

import           ML.BORL      as B hiding (actionFilter, featureExtractor)
import           SimSim

-- useHeuristicToFillReplMem :: Maybe Release
-- useHeuristicToFillReplMem = Just $ releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])


-- | BORL Parameters.
borlParams :: Parameters Double
borlParams = Parameters
  { _alpha               = 0.01
  , _alphaANN            = 1.0
  , _beta                = 0.03
  , _betaANN             = 1.0
  , _delta               = 0.01
  , _deltaANN            = 1.0
  , _gamma               = 0.01
  , _gammaANN            = 1.0
  , _epsilon             = 5    -- was 2
  , _explorationStrategy = SoftmaxBoltzmann 2
  , _exploration         = 1.0
  , _learnRandomAbove    = 0.50
  , _zeta                = 0.01
  , _xi                  = 0.01
  , _disableAllLearning  = False
  }


nnConfig :: NNConfig
nnConfig =
  NNConfig
    { _replayMemoryMaxSize             = 100000 -- was 30k
    , _trainBatchSize                  = 24
    , _grenadeLearningParams           = LearningParameters 0.01 0.0 0.0001
    , _learningParamsDecay             = ExponentialDecay Nothing 0.15 100000 -- was (Just 10^-5)
    , _prettyPrintElems                = [] -- is set just before printing
    , _scaleParameters                 = ScalingNetOutParameters (-500) 500 (-5000) 5000 (-5000) 5000 (-5000) 5000
    , _stabilizationAdditionalRho      = 0
    , _stabilizationAdditionalRhoDecay = ExponentialDecay Nothing 0.05 100000
    , _updateTargetInterval            = 1
    , _trainMSEMax                     = Nothing -- Just 0.03
    , _setExpSmoothParamsTo1           = True
    }


alg :: Algorithm s
alg =
  -- AlgDQNAvgRewardFree 0.75 0.995 ByStateValues
  AlgBORL defaultGamma0 defaultGamma1 ByStateValues Nothing
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)
  -- algDQN

initVals :: InitValues
initVals = InitValues 0 0 0 0 0

experimentName :: T.Text
experimentName = "31.12. Adaptive BORL Order Releaser with unif procTimes, unif demand"

