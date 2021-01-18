{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Releaser.Decay.Ops where


import           ML.BORL

import           Releaser.Decay.Type
import           Releaser.Util

-- | Decay function of parameters.d
decayRateStepsWith :: DecayRate -> DecaySteps -> ConfigDecay
decayRateStepsWith rate steps =
  ConfigDecay rate steps ("Exponential decay with rate " <> tshow rate <> " in " <> tshow steps <> " steps") $
  Parameters
    { _alpha            = ExponentialDecay (Just 5e-5) rate steps -- HERE CHANGE MIN???
    , _alphaRhoMin      = ExponentialDecay (Just 2e-5) rate steps
    , _beta             = ExponentialDecay (Just 1e-4) rate steps
    , _delta            = ExponentialDecay (Just 5e-4) rate steps
    , _gamma            = ExponentialDecay (Just 1e-3) rate steps
    , _epsilon          = [NoDecay]
    , _exploration      = ExponentialDecay (Just 0.005) rate steps -- was (Just 0.20)
      -- not used in AverageRewardAdjusted
    , _learnRandomAbove = NoDecay
    , _zeta             = NoDecay -- ExponentialDecay (Just 1e-5) rate steps -- was (Just 1e-5)
    , _xi               = NoDecay
    }
