
module Releaser.Settings
    ( module S
    ) where


import           Releaser.SettingsCosts  as S
import           Releaser.SettingsDemand as S
import           Releaser.SettingsPeriod as S


-- ----------------------------------------

-- costConfig :: Costs
-- costConfig = Costs 3 10 20

-- actionFilter :: ListOfActions -> Reader ActionFilterPLTConfig (St -> [Bool])
-- actionFilter = actionFilterPLT

-- demand :: Demand
-- demand = demandUniformIn3To15FixedDds


-- actionConfig :: ActionConfig
-- actionConfig = ActionConfig
--   { actLowerActionBound = -1
--   , actUpperActionBound = 1
--   , actPeriodLength     = periodLength
--   , actProductTypes     = ptTypes
--   }

-- actionFilterConfig :: ActionFilterPLTConfig
-- actionFilterConfig = ActionFilterPLTConfig
--   { actFilMinimumPLT   = 1
--   , actFilMaximumPLT   = 7
--   , actFilPeriodLength = periodLength
--   }
