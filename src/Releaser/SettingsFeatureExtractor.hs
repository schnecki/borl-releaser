

module Releaser.SettingsFeatureExtractor
    ( featureExtractor
    , ReduceValues
    , extractFeatures
    , ConfigFeatureExtractor (..)
    , scalePltsMin
    , scalePltsMax
    , scaleOrderMin
    , scaleOrderMax
    ) where

import           Releaser.FeatureExtractor.Ops
import           Releaser.FeatureExtractor.Type
import           Releaser.Type

featureExtractor :: ReduceValues -> ConfigFeatureExtractor
featureExtractor = -- featExtractorSimple

  -- HERE
  featExtractorFullMachinesToQueue
  -- featExtractorFullMachinesToQueue  -- was this


  -- featExtractorFullMachinesToQueueNbnBn
  -- featExtractorSimpleWipWithQueueCountsAndMachineCount -- for testing

  -- featExtractorFullWithMachines
  -- NOTE: Currently the agents has no information on backorders while they are in the system!!!

 -- featExtractorSimpleWipWithQueueCounts   -- <- has been working up to -55 costs with const demand
 -- featExtractorSimpleWipWithQueueCountsAndMachineCount -- <- works!!!

  -- featextractorsimplewithqueuecounts -- (no FGI does not make sense)
  -- featExtractorWipAsQueueCounters


extractFeatures :: ReduceValues -> St -> Extraction
extractFeatures = configFeatureExtractor . featureExtractor
