

module Releaser.ActionFilter.Type
    ( ActionFilterConfig (..)
    ) where


import           SimSim

data ActionFilterConfig = ActionFilterConfig
  { configActFilterMin :: Integer
  , configActFilterMax :: Integer
  }


