{-# LANGUAGE BangPatterns #-}

module Releaser.Demand.Type where

import           Data.Text (Text)

import           SimSim

data ConfigDemand = ConfigDemand
  { configDemandName            :: !Text
  , configDemandMaxDueDateSlack :: !Integer -- In Periods
  , configDemandFunction        :: !(SimSim -> IO [Order])
  }
