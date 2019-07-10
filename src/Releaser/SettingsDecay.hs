{-# LANGUAGE OverloadedStrings #-}

module Releaser.SettingsDecay
    ( decay
    , ConfigDecay (..)
    ) where

import           ML.BORL

import           Releaser.Decay.Ops
import           Releaser.Decay.Type

decay :: ConfigDecay
decay = decayExpXiIsBetaHalf


exponentialDecayWithRate50And150kSteps :: ConfigDecay
exponentialDecayWithRate50And150kSteps = ConfigDecay "Exponential Decay with rate 0.50 and 150000 steps"
  (exponentialDecay Nothing 0.50 150000) -- TODO add minimum
