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
import           Releaser.SettingsReward
import           Releaser.SettingsRouting (bnNbn, productTypes)
import           Releaser.Type

borlSettings :: Settings
borlSettings =
  Settings
    { _useProcessForking             = True
    , _disableAllLearning            = False
    , _explorationStrategy           = EpsilonGreedy
    , _nStep                         = 5
    , _mainAgentSelectsGreedyActions = False -- True
    , _workersMinExploration         = take 8 $ 0.01 : 0.02 : 0.03 : 0.04 : [0.05, 0.10 .. 1.0] -- DIFFERENT ONES?
    , _overEstimateRho               = False
    , _independentAgents             = if bnNbn then 2 else length productTypes
    , _independentAgentsSharedRho    = True
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
  , _epsilon             = [0.25, 0.25] -- If epsilon is too big, R0 will decrease the LT to collect more reward sooner!!!
  , _exploration         = 1.0
  , _learnRandomAbove    = 1.0 -- 0.8
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
    , _grenadeLearningParams           = OptAdam 0.005 0.9 0.999 1e-8 1e-3
    , _grenadeSmoothTargetUpdate       = 0.01
    , _grenadeSmoothTargetUpdatePeriod = 100
    , _learningParamsDecay             = ExponentialDecay (Just 5e-6) (configDecayRate decay) (configDecaySteps decay)
    , _prettyPrintElems                = []      -- is set just before printing/at initialisation
    , _scaleParameters                 = ScalingNetOutParameters (-800) 800 (-300) 300 (-400) 800 (-400) 800
    , _scaleOutputAlgorithm            = ScaleMinMax
    , _cropTrainMaxValScaled           = Nothing -- Just 0.98
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
  AlgDQNAvgRewAdjusted 0.8 1.0 ByStateValues
  -- AlgDQNAvgRewAdjusted 0.99 1.0 ByStateValues
  -- (ByStateValuesAndReward 0.5 NoDecay)
  -- (ByMovAvg 5000)
  -- algDQN

initVals :: InitValues
initVals = InitValues {defaultRhoMinimum = 300, defaultRho = 0, defaultV = 0, defaultW = 0, defaultR0 = 0, defaultR1 = 0}


experimentName :: T.Text
experimentName = "Final runs: 3 stage setup on 13/01/2021"


scaleAlg :: ScalingAlgorithm
scaleAlg = nnConfig ^. scaleOutputAlgorithm
