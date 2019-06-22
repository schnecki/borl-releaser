

module Releaser.ActionFilter
    ( actionFilterPLT
    , ActionFilterPLTConfig (..)
    ) where

import           Control.Monad.Trans.Reader
import qualified Data.Map.Strict            as M

import           SimSim

import           Releaser.Type


data ActionFilterPLTConfig = ActionFilterPLTConfig
  { actFilMinimumPLT   :: Integer
  , actFilMaximumPLT   :: Integer
  , actFilPeriodLength :: Time
  }


actionFilterPLT :: ListOfActions -> Reader ActionFilterPLTConfig (St -> [Bool])
actionFilterPLT acts = do
  (ActionFilterPLTConfig minPLTPeriod maxPLTPeriod periodLen) <- ask
  let minPLT = fromIntegral minPLTPeriod * periodLen
      maxPLT = fromIntegral maxPLTPeriod * periodLen
  return $ \(St _ _ _ plts) -> map (isInBounds minPLT maxPLT plts) acts
  where
    isInBounds minPLT maxPLT plts act = all (\x -> x >= minPLT && x <= maxPLT) $ zipWith (+) (M.elems plts) act
