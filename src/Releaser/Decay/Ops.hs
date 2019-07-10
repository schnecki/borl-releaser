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
        (max 0.03 $ decay slow * alp)
        (max 0.015 $ decay slow * bet)
        (max 0.015 $ decay slow * del)
        (max 0.015 $ decay slow * ga)
        (max 0.05 $ decay slow * eps)
        (max 0.05 $ decay slower * exp)
        rand
        zeta
        (0.5 * bet)
      where
        slower = 0.01
        slow = 0.005
        decaySteps = 150000 :: Double
        decay rate = rate ** (fromIntegral t / decaySteps)

