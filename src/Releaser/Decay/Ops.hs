{-# LANGUAGE OverloadedStrings #-}

module Releaser.Decay.Ops where


import           ML.BORL

import           Releaser.Decay.Type

-- | Decay function of parameters.
decayExpXiIsBetaHalf :: ConfigDecay
decayExpXiIsBetaHalf = ConfigDecay "Exponential Decay with xi=0.5*beta"  decayExp
  where
    decayExp t p@(Parameters alp bet del ga eps exp rand zeta xi) =
      Parameters
        (max 0.03 $ decay rate * alp)
        (max 0.015 $ decay rate * bet)
        (max 0.015 $ decay rate * del)
        (max 0.015 $ decay rate * ga)
        (max 0.1 $ decay rate * eps)
        (max 0.1 $ decay rate * exp)
        rand
        zeta
        (max 0.015 $ 0.5 * bet)
      where
        rate = 0.05             -- will be reached after `decaySteps` rounds
        decaySteps = 750000 :: Double
        decay rate = rate ** (fromIntegral t / decaySteps)

