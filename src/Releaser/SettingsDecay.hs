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
decay =
  -- decayRateStepsWith 0.6 100000 -- (3*10^6)
  -- decayRate50PctStepsk150k
  -- decayRateStepsWith 0.55 50000 -- (10^5)
  decayRateStepsWith 0.25 100000

  -- decayRateStepsWith 0.5 100000 -- (3*10^6)
