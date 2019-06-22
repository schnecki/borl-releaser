{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Releaser.ReleasePLT
    ( mkReleasePLT
    , pltReleaseName
    ) where

import           ClassyPrelude
import qualified Data.Map      as M

import           SimSim

import           Releaser.Type


pltReleaseName :: Text
pltReleaseName = "PLT"

mkReleasePLT :: PLTs -> Release
mkReleasePLT plts = Release (releaseFun plts) pltReleaseName

releaseFun :: PLTs -> Time -> [Order] -> IO [Order]
releaseFun plts t orders = return $ filter isInPlt orders
  where isInPlt order = case M.lookup (productType order) plts of
          Nothing -> error $ "Could not find planned lead time for product type: " ++ show (productType order)
          Just plt -> dueDate order - plt <= t


