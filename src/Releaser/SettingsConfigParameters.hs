{-# LANGUAGE OverloadedLists   #-}
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

borlSettings :: Settings
borlSettings =
  Settings
    { _explorationStrategy = EpsilonGreedy -- SoftmaxBoltzmann 5
    , _disableAllLearning = False
    , _useForking = True
    }


-- | BORL Parameters.
borlParams :: Parameters Float
borlParams = Parameters
  { _alpha               = 0.01
  , _alphaRhoMin         = 1e-5
  , _beta                = 0.01
  , _delta               = 0.005
  , _gamma               = 0.01
  -- Rest
  , _epsilon             = [0.30, 0.01] -- If epsilon is too big, R0 will decrease the LT to collect more reward sooner!!!
  , _exploration         = 1.0
  , _learnRandomAbove    = 0.5
  -- Multichain NBORL and etc.
  , _zeta                = 0.10
  , _xi                  = 5e-3
  }


nnConfig :: NNConfig
nnConfig =
  NNConfig
  {   _replayMemoryMaxSize             = 20000 -- was 30k
    , _replayMemoryStrategy            = ReplayMemoryPerAction -- ReplayMemorySingle
    , _trainBatchSize                  = 8 -- 32
    , _grenadeLearningParams           = OptAdam 0.001 0.9 0.999 1e-7
    , _learningParamsDecay             = ExponentialDecay Nothing 0.85 50000
    , _prettyPrintElems                = [] -- is set just before printing/at initialisation
    , _scaleParameters                 = ScalingNetOutParameters (-800) 800 (-5000) 5000 (-3000) 3000 (-3000) 3000
    , _stabilizationAdditionalRho      = 0
    , _stabilizationAdditionalRhoDecay = ExponentialDecay Nothing 0.05 75000
    , _updateTargetInterval            = 10000
    , _updateTargetIntervalDecay       = StepWiseIncrease (Just 500) 0.1 10000
    , _workersMinExploration           = -- [0.5, 0.3, 0.15, 0.10, 0.05, 0.025, 0.01]
        [0.10, 0.05, 0.025, 0.01]
    }

------------------------------ ###########################################
-- |!!!!! TODO: initial states for experiments have to be independent on this selection!!!!!
------------------------------ ###########################################
alg :: Algorithm s
alg =
  -- AlgBORL defaultGamma0 defaultGamma1 ByStateValues Nothing
  -- AlgDQNAvgRewAdjusted 0.8 0.995 (ByStateValuesAndReward 1.0 (ExponentialDecay (Just 0.8) 0.99 100000))
  -- AlgDQNAvgRewAdjusted 0.75 0.99 ByStateValues
  AlgDQNAvgRewAdjusted 0.75 1.00 ByStateValues -- (Fixed 150)
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)
  -- algDQN

initVals :: InitValues
initVals = InitValues {defaultRhoMinimum = 150, defaultRho = 350, defaultV = 0, defaultW = 0, defaultR0 = 0, defaultR1 = 0}

experimentName :: T.Text
experimentName = "20.01.2020 Adaptive BORL Order Releaser with unif procTimes, unif demand"


