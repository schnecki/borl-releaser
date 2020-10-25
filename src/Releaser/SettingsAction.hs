
module Releaser.SettingsAction
  ( actionConfig
  , action
  , ActionConfig (..)
  , Act
  ) where

import           Control.Monad.Trans.Reader

import           ML.BORL
import           SimSim                     hiding (productTypes)

import           Releaser.Action.ActionPlt
import           Releaser.Action.Type
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type


-- ----------------------------------------
action :: ActionFunction St Act
action = actionFun


actionConfig :: ActionConfig
actionConfig = ActionConfig
  { configActLower = -1
  , configActUpper = 1
  }
