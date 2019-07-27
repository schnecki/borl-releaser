{-# LANGUAGE OverloadedStrings #-}

module Releaser.Decay.Ops where


import           ML.BORL

import           Releaser.Decay.Type
import           Releaser.Util

-- -- | Decay function of parameters.
-- decayExpXiIsBetaHalf :: ConfigDecay
-- decayExpXiIsBetaHalf = ConfigDecay "Exponential Decay with xi=0.5*beta"  decayExp
--   where
--     decayExp t p@(Parameters alp alpANN bet betANN del delANN ga gaANN eps exp rand zeta xi dis) =
--       Parameters
--         (max 0.03 $ decay rate * alp)
--         (max 0.015 $ decay rate * bet)
--         (max 0.015 $ decay rate * del)
--         (max 0.015 $ decay rate * ga)
--         (max 0.1 $ decay rate * eps)
--         (max 0.1 $ decay rate * exp)
--         rand
--         zeta
--         (max 0.015 $ 0.5 * bet)
--         dis
--       where
--         rate = 0.05             -- will be reached after `decaySteps` rounds
--         decaySteps = 350000 :: Double
--         decay rate = rate ** (fromIntegral t / decaySteps)


decayRate10PctSteps300k :: ConfigDecay
decayRate10PctSteps300k = ConfigDecay ("Exponential decay with rate " <> tshow rate <> " in " <> tshow rate <> " steps") dec
  where rate = 0.10
        steps = 300000
        dec = exponentialDecay (Just minValues) rate steps
        minValues =
         Parameters
           { _alpha = 0.000
           , _alphaANN = 0      -- not decayed
           , _beta =  0.005
           , _betaANN = 0       -- not decayed
           , _delta = 0.005
           , _deltaANN = 0      -- not decayed
           , _gamma = 0.005
           , _gammaANN = 0      -- not decayed
           , _epsilon = 0.05
           , _exploration = 0.05
           , _learnRandomAbove = 0.05
           , _zeta = 0.0
           , _xi = 0.0075
           , _disableAllLearning = False
           }
