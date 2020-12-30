module Releaser.SettingsCosts
  ( costConfig
  , ConfigCosts(..)
  ) where


import           Releaser.Costs.Type

----------------------------------------

costConfig :: ConfigCosts
costConfig = ConfigCosts 1 4 16   -- Use this one for the evaluations!!!
-- costConfig = ConfigCosts 3 10 20
