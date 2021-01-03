{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}
module Releaser.SettingsConfigParameters where

import           Control.Lens             ((^.))
import qualified Data.Text                as T


-- ANN modules
import           Grenade

import           ML.BORL                  as B hiding (actionFilter, featureExtractor)
import           Releaser.SettingsDecay
import           Releaser.SettingsRouting (productTypes)

-- useHeuristicToFillReplMem :: Maybe Release
-- useHeuristicToFillReplMem = Just $ releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])

borlSettings :: Settings
borlSettings =
  Settings
    { _useProcessForking             = True
    , _disableAllLearning            = False
    , _explorationStrategy           = EpsilonGreedy -- SoftmaxBoltzmann 5
    , _nStep                         = 5
    , _mainAgentSelectsGreedyActions = False
    , _workersMinExploration         = replicate 7 0.01 ++ [0.05, 0.10, 0.20, 0.30]
    , _overEstimateRho               = False -- True
    , _independentAgents             = length productTypes
    , _independentAgentsSharedRho    = True -- False
    }


-- | BORL Parameters.
borlParams :: Parameters Double
borlParams = Parameters
  { _alpha               = 0.01
  , _alphaRhoMin         = 0 -- 0.001
  , _beta                = 0.01
  , _delta               = 0.005
  , _gamma               = 0.01
  -- Rest
  , _epsilon             = [0.50, 0.30] -- If epsilon is too big, R0 will decrease the LT to collect more reward sooner!!!
  , _exploration         = 1.0
  , _learnRandomAbove    = 0.8
  -- Multichain NBORL and etc.
  , _zeta                = 0.10
  , _xi                  = 5e-3
  }


nnConfig :: NNConfig
nnConfig =
  NNConfig
  {   _replayMemoryMaxSize             = 10800 -- 2700 -- 5400 -- 2700
    , _replayMemoryStrategy            = ReplayMemoryPerAction -- ReplayMemorySingle
    , _trainBatchSize                  = 4
    , _trainingIterations              = 1
    , _grenadeLearningParams           = OptAdam 0.001 0.9 0.999 1e-8 1e-3
    , _grenadeSmoothTargetUpdate       = 0.01
    , _grenadeSmoothTargetUpdatePeriod = 100
    , _learningParamsDecay             = ExponentialDecay (Just 1e-6) 0.75 10000 -- (configDecayRate decay) -- (round $ 2 * fromIntegral (configDecaySteps decay))
    , _prettyPrintElems                = []      -- is set just before printing/at initialisation
    , _scaleParameters                 = ScalingNetOutParameters (-800) 800 (-300) 300 (-1000) 1000 (-1000) 1000
    , _scaleOutputAlgorithm            = ScaleMinMax -- ScaleLog 150 -- TODO ScaleMinMax --
    , _cropTrainMaxValScaled           = Just 0.98
    , _grenadeDropoutFlipActivePeriod  = 10^5
    , _grenadeDropoutOnlyInactiveAfter = 0 -- 10^6
    , _clipGradients                   = NoClipping -- ClipByGlobalNorm 0.01
    }

------------------------------ ###########################################
-- |!!!!! TODO: initial states for experiments have to be independent on this selection!!!!!
------------------------------ ###########################################
alg :: Algorithm s
alg =
  -- AlgBORL defaultGamma0 defaultGamma1 ByStateValues Nothing
  -- AlgDQNAvgRewAdjusted 0.8 0.995 (ByStateValuesAndReward 1.0 (ExponentialDecay (Just 0.8) 0.99 100000))
  -- AlgDQNAvgRewAdjusted 0.75 0.99 ByStateValues
  -- AlgDQNAvgRewAdjusted 0.8 0.995 ByStateValues
  AlgDQNAvgRewAdjusted 0.99 1.0 ByStateValues
  -- AlgDQNAvgRewAdjusted 0.99 1.0 ByStateValues
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)
  -- algDQN

initVals :: InitValues
initVals = InitValues {defaultRhoMinimum = 300, defaultRho = 120, defaultV = 0, defaultW = 0, defaultR0 = 0, defaultR1 = 0}

experimentName :: T.Text
experimentName = "3 stage setup 01.01.2021"


scaleAlg :: ScalingAlgorithm
scaleAlg = nnConfig ^. scaleOutputAlgorithm
