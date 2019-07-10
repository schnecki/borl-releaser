
module Releaser.SettingsAction
  ( actionConfig
  , action
  , ActionConfig (..)
  ) where

import           Control.Monad.Trans.Reader

import           ML.BORL
import           SimSim                     hiding (productTypes)

import           Releaser.Action.ActionPlt
import           Releaser.SettingsPeriod
import           Releaser.SettingsRouting
import           Releaser.Type


-- ----------------------------------------
action :: St -> Reader ActionConfig (ListOfActions, [Action St])
action = actionsPLT


actionConfig :: ActionConfig
actionConfig = ActionConfig
  { configActLower = -1
  , configActUpper = 1
  }

