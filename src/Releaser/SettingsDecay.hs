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
  decayRateStepsWith 0.5 25000
  -- decayRateStepsWith 0.5 100000 -- (3*10^6)

