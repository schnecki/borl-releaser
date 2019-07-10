

module Releaser.Decay.Type where


import           Data.Text (Text)

import           ML.BORL

data ConfigDecay = ConfigDecay
  { configDecayName :: Text
  , configDecay     :: Decay
  }
