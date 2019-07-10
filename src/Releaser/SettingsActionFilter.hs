
module Releaser.SettingsActionFilter
    ( actionFilterConfig
    , actionFilter
    , ActionFilterConfig (..)
    ) where

import           Control.Monad.Trans.Reader


import           Releaser.ActionFilter.Ops
import           Releaser.SettingsPeriod
import           Releaser.Type

-- ----------------------------------------

actionFilter :: ListOfActions -> Reader ActionFilterConfig (St -> [Bool])
actionFilter = actionFilterMinMax

actionFilterConfig :: ActionFilterConfig
actionFilterConfig = ActionFilterConfig
  { configActFilterMin   = 1
  , configActFilterMax   = 7
  }
