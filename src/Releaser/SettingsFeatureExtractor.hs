

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
  -- featExtractorSimpleWipWithQueueCounts
  featExtractorSimpleWipWithQueueCountsAndMachineCount
  -- featExtractorSimpleWithQueueCounts (not FGI does not make sense)
  -- featExtractorWipAsQueueCounters


extractFeatures :: ReduceValues -> St -> Extraction
extractFeatures = configFeatureExtractor . featureExtractor
