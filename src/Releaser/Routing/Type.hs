{-# LANGUAGE BangPatterns #-}

module Releaser.Routing.Type where

import           Data.Text (Text)

import           SimSim

data ConfigRouting = ConfigRouting
  { configRoutingName   :: !Text
  , configRoutingRoutes :: !Routes
  }
