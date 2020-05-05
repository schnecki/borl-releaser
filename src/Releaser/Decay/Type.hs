

module Releaser.Decay.Type where


import           Data.Text (Text)

import           ML.BORL

data ConfigDecay = ConfigDecay
  { configDecayRate  :: !DecayRate
  , configDecaySteps :: !DecaySteps
  , configDecayName  :: !Text
  , configDecay      :: !ParameterDecaySetting
  }
