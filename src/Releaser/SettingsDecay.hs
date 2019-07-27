{-# LANGUAGE OverloadedStrings #-}

module Releaser.SettingsDecay
    ( decay
    , ConfigDecay (..)
    ) where

import           ML.BORL

import           Releaser.Decay.Ops
import           Releaser.Decay.Type
import           Releaser.Util

decay :: ConfigDecay
decay = decayGridworld


decayGridworld :: ConfigDecay
decayGridworld = ConfigDecay ("Exponential decay with rate " <> tshow rate <> " in " <> tshow rate <> " steps") dec
  where rate = 0.10
        steps = 300000
        dec = exponentialDecay (Just minValues) rate steps
        minValues =
         Parameters
           { _alpha = 0.000
           , _alphaANN = 0      -- not decayed
           , _beta =  0.005
           , _betaANN = 0       -- not decayed
           , _delta = 0.005
           , _deltaANN = 0      -- not decayed
           , _gamma = 0.005
           , _gammaANN = 0      -- not decayed
           , _epsilon = 0.05
           , _exploration = 0.05
           , _learnRandomAbove = 0.05
           , _zeta = 0.0
           , _xi = 0.0075
           , _disableAllLearning = False
           }
