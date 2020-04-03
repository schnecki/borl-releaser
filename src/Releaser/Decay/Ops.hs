{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Releaser.Decay.Ops where


import           ML.BORL

import           Releaser.Decay.Type
import           Releaser.Util

-- | Decay function of parameters.
decayRateStepsWith :: DecayRate -> DecaySteps -> ConfigDecay
decayRateStepsWith rate steps = ConfigDecay ("Exponential decay with rate " <> tshow rate <> " in " <> tshow steps <> " steps") dec
  where
    dec =
      decaySetupParameters
        Parameters
          { _alpha            = ExponentialDecay (Just 1e-7) 0.25 steps
          , _alphaRhoMin      = NoDecay
          , _beta             = ExponentialDecay (Just 1e-4) rate steps
          , _delta            = ExponentialDecay (Just 5e-4) rate steps
          , _gamma            = ExponentialDecay (Just 1e-3) rate steps
          , _zeta             = NoDecay -- ExponentialDecay (Just 1e-5) rate steps -- was (Just 1e-5)
          , _xi               = NoDecay
          -- Exploration
          , _epsilon          = [NoDecay] -- ExponentialDecay (Just 0.50) rate steps
          , _exploration      = ExponentialDecay Nothing rate steps -- was (Just 0.20)
          , _learnRandomAbove = NoDecay
          }
