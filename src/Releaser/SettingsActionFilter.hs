
module Releaser.SettingsActionFilter
    ( actionFilterConfig
    , actionFilter
    , ActionFilterConfig (..)
    ) where

import           Control.Monad.Trans.Reader
import qualified Data.Vector.Storable       as V

import           Releaser.ActionFilter.Ops
import           Releaser.SettingsDemand
import           Releaser.SettingsPeriod
import           Releaser.Type

-- ----------------------------------------

actionFilter :: Reader ActionFilterConfig (St -> [V.Vector Bool])
actionFilter = actionFilterMinMax

actionFilterConfig :: ActionFilterConfig
actionFilterConfig = ActionFilterConfig
  { configActFilterMin   = 1
  , configActFilterMax   = configDemandMaxDueDateSlack demand
  }
