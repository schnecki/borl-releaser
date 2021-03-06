

module Releaser.ActionFilter.Ops
    ( actionFilterMinMax
    , ActionFilterConfig (..)
    ) where

import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Storable       as V

import           Releaser.Action.Type
import           Releaser.ActionFilter.Type
import           Releaser.SettingsPeriod
import           Releaser.Type

import           SimSim


-- actionFilterMinMax :: ListOfActions -> Reader ActionFilterConfig (St -> V.Vector Bool)
-- actionFilterMinMax acts = do
--   (ActionFilterConfig minPLTPeriod maxPLTPeriod) <- ask
--   let minPLT = fromIntegral minPLTPeriod * periodLength
--       maxPLT = fromIntegral maxPLTPeriod * periodLength
--   return $ \(St _ _ _ plts) -> V.fromList $ map (isInBounds minPLT maxPLT plts) acts
--   where
--     isInBounds minPLT maxPLT plts act = all (\x -> x >= minPLT && x <= maxPLT) $ zipWith (+) (M.elems plts) act


actionFilterMinMax :: Reader ActionFilterConfig (St -> [V.Vector Bool])
actionFilterMinMax = do
  (ActionFilterConfig minPLTPeriod maxPLTPeriod) <- ask
  let minPLT = fromIntegral minPLTPeriod * periodLength
      maxPLT = fromIntegral maxPLTPeriod * periodLength
  return $ \(St _ _ _ plts) -> map (isInBounds minPLT maxPLT [minBound .. maxBound]) (M.elems plts)
  where
    isInBounds :: Time -> Time -> [ActIndepAgents] -> Time -> V.Vector Bool
    isInBounds minPLT maxPLT acts t =
      V.fromList $
      map
        (\a ->
           let t' = applyActIndepAgents a t
            in t' >= minPLT && t' <= maxPLT)
        acts
