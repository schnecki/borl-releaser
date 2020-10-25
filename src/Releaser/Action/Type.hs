{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Releaser.Action.Type
    ( ActionConfig (..)
    , Act
    , ActIndepAgents (..)
    , applyActIndepAgents
    ) where

import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics

import           SimSim

import           Releaser.SettingsPeriod

type Act = ActIndepAgents

data ActIndepAgents = DecLT | NoChg | IncLT
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, Serialize)

applyActIndepAgents :: ActIndepAgents -> Time -> Time
applyActIndepAgents DecLT = subtract periodLength
applyActIndepAgents NoChg = id
applyActIndepAgents IncLT = (+ periodLength)


data ActionConfig =
  ActionConfig
    { configActLower :: !Integer
    , configActUpper :: !Integer
    }
