

module Releaser.SettingsFeatureExtractor
    ( featureExtractor
    , ReduceValues
    , extractFeatures
    , ConfigFeatureExtractor (..)
    ) where

import           Releaser.FeatureExtractor.Ops
import           Releaser.FeatureExtractor.Type
import           Releaser.Type

featureExtractor :: ReduceValues -> ConfigFeatureExtractor
featureExtractor = -- featExtractorSimple
  featExtractorSimpleWipWithQueueCounts
  -- featExtractorSimpleWithQueueCounts (not FGI does not make sense)
  -- featExtractorWipAsQueueCounters


extractFeatures :: ReduceValues -> St -> Extraction
extractFeatures = configFeatureExtractor . featureExtractor
