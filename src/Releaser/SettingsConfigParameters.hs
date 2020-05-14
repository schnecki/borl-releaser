{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
module Releaser.SettingsConfigParameters where

import qualified Data.Text              as T

-- ANN modules
import           Grenade

import           ML.BORL                as B hiding (actionFilter, featureExtractor)
import           Releaser.SettingsDecay

-- useHeuristicToFillReplMem :: Maybe Release
-- useHeuristicToFillReplMem = Just $ releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])

borlSettings :: Settings
borlSettings =
  Settings
    { _useProcessForking = True
    , _disableAllLearning = False
    , _explorationStrategy = EpsilonGreedy -- SoftmaxBoltzmann 5
    , _nStep = 3
    , _mainAgentSelectsGreedyActions = False
    , _workersMinExploration = replicate 5 0.01 ++ [0.1, 0.2] -- ++ [0.5, 0.3, 0.15, 0.10, 0.05, 0.025]
        -- [0.10, 0.05, 0.025, 0.01]
      -- replicate 12 0.01
    }


-- | BORL Parameters.
borlParams :: Parameters Float
borlParams = Parameters
  { _alpha               = 0.01
  , _alphaRhoMin         = 0.001
  , _beta                = 0.01
  , _delta               = 0.005
  , _gamma               = 0.01
  -- Rest
  , _epsilon             = [0.30, 0.50] -- If epsilon is too big, R0 will decrease the LT to collect more reward sooner!!!
  , _exploration         = 1.0
  , _learnRandomAbove    = 0.97
  -- Multichain NBORL and etc.
  , _zeta                = 0.10
  , _xi                  = 5e-3
  }


nnConfig :: NNConfig
nnConfig =
  NNConfig
  {   _replayMemoryMaxSize             = 1000 -- 20000 -- was 30k
    , _replayMemoryStrategy            = ReplayMemoryPerAction -- ReplayMemorySingle
    , _trainBatchSize                  = 4
    , _trainingIterations              = 1
    , _grenadeLearningParams           = OptAdam 0.005 0.9 0.999 1e-7 1e-3
    , _grenadeSmoothTargetUpdate       = 0.01
    , _learningParamsDecay             = ExponentialDecay (Just 5e-6) (configDecayRate decay) (round $ 2 * fromIntegral (configDecaySteps decay))
    , _prettyPrintElems                = []      -- is set just before printing/at initialisation
    , _scaleParameters                 = ScalingNetOutParameters (-800) 800 (-5000) 5000 (-2000) 2000 (-5000) 5000
    , _cropTrainMaxValScaled           = Just 0.98 -- Nothing
    , _grenadeDropoutFlipActivePeriod  = 100000
    , _grenadeDropoutOnlyInactiveAfter = 0 -- 10^6
    , _updateTargetInterval            = 10000
    , _updateTargetIntervalDecay       = StepWiseIncrease (Just 500) 0.1 10000
    }

------------------------------ ###########################################
-- |!!!!! TODO: initial states for experiments have to be independent on this selection!!!!!
------------------------------ ###########################################
alg :: Algorithm s
alg =
  -- AlgBORL defaultGamma0 defaultGamma1 ByStateValues Nothing
  -- AlgDQNAvgRewAdjusted 0.8 0.995 (ByStateValuesAndReward 1.0 (ExponentialDecay (Just 0.8) 0.99 100000))
  -- AlgDQNAvgRewAdjusted 0.75 0.99 ByStateValues
  AlgDQNAvgRewAdjusted 0.75 1.0 ByStateValues -- (Fixed 150)
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)
  -- algDQN

initVals :: InitValues
initVals = InitValues {defaultRhoMinimum = 500, defaultRho = 120, defaultV = 0, defaultW = 0, defaultR0 = 0, defaultR1 = 0}

experimentName :: T.Text
experimentName = "20.01.2020 Adaptive BORL Order Releaser with unif procTimes, unif demand"


