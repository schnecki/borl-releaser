{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


module Releaser.Action.Type
    ( ActionConfig (..)
    ) where


import           SimSim


data ActionConfig = ActionConfig
  { configActLower :: !Integer
  , configActUpper :: !Integer
  }


