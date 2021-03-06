{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Releaser.Release.ReleasePlt
    ( mkReleasePLT
    , pltReleaseName
    ) where

import           ClassyPrelude
import qualified Data.Map                 as M

import           SimSim

import           Releaser.SettingsRouting
import           Releaser.Type


pltReleaseName :: Text
pltReleaseName = "PLT by RL Agent"

mkReleasePLT :: LTs -> Release
mkReleasePLT plts = Release (releaseFun plts) pltReleaseName

releaseFun :: LTs -> Time -> [Order] -> IO [Order]
releaseFun plts t orders = return $ filter isInPlt orders
  where isInPlt order = case M.lookup (mapProductType $ productType order) plts of
          Nothing -> error $ "Could not find planned lead time for product type: " ++ show (productType order)
          Just plt -> dueDate order - plt <= t
