{-# LANGUAGE OverloadedStrings #-}

module Releaser.SettingsDecay
    ( decay
    , ConfigDecay (..)
    ) where

import           ML.BORL

import           Releaser.Decay.Ops
import           Releaser.Decay.Type

decay :: ConfigDecay
decay =
  ConfigDecay "Exponential Decay with rate 0.96 and 750000 steps"
  (exponentialDecay Nothing 0.96 750000)
