{-# LANGUAGE OverloadedStrings #-}

module Releaser.SettingsDecay
    ( decay
    , ConfigDecay (..)
    ) where

import           ML.BORL

import           Releaser.Decay.Ops
import           Releaser.Decay.Type

decay :: ConfigDecay
decay = decayGridworld


decayGridworld :: ConfigDecay
decayGridworld = ConfigDecay "Exponential decay with rate 0.10 in 300k steps" dec
  where dec = exponentialDecay (Just minValues) 0.10 300000
        minValues =
         Parameters
           { _alpha = 0.000
           , _beta =  0.005
           , _delta = 0.005
           , _gamma = 0.005
           , _epsilon = 0.05
           , _exploration = 0.05
           , _learnRandomAbove = 0.05
           , _zeta = 0.0
           , _xi = 0.0075
           , _disableAllLearning = False
           }
