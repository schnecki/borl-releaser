{-# LANGUAGE OverloadedStrings #-}

module Releaser.Demand.Ops
  ( demandUniformIn3To15FixedDds
  , demandConst9FixedDds
  , demandConst10FixedDds
  , demandConst11FixedDds
  , demandUnif95_175
  , demandUnif78_158
  , demandExp135
  , demandExp118
  , demandExp105
  ) where


import           Statistics.Distribution.Exponential
import           Statistics.Distribution.Uniform

import           SimSim

import           Releaser.Demand.Type
import           Releaser.SettingsPeriod
import           Releaser.Util

dds :: Integer
dds = 7

dueDateSlack :: Time
dueDateSlack = fromInteger dds * periodLength


demandUniformIn3To15FixedDds :: ConfigDemand
demandUniformIn3To15FixedDds = ConfigDemand ("U(3,15) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 3 15 dueDateSlack)

demandConst9FixedDds :: ConfigDemand
demandConst9FixedDds = ConfigDemand ("Const(9) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 9 9 dueDateSlack)

demandConst10FixedDds :: ConfigDemand
demandConst10FixedDds = ConfigDemand ("Const(10) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 10 10 dueDateSlack)


demandConst11FixedDds :: ConfigDemand
demandConst11FixedDds = ConfigDemand ("Const(11) with DDS=" <> tshow dueDateSlack) dds (\sim -> generateOrdersUniform sim 11 11 dueDateSlack)


demandUnif95_175 :: ConfigDemand
demandUnif95_175 =
  ConfigDemand
    ("Uniform interarrival-time w/ Unif(95,175) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
        in generateOrdersFixedDueDateSlack sim (uniformDistr 0.098958333 0.182291667) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)

demandUnif78_158 :: ConfigDemand
demandUnif78_158 =
  ConfigDemand
    ("Uniform interarrival-time w/ Unif(78,158) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
        in generateOrdersFixedDueDateSlack sim (uniformDistr (78 / 960) (158 / 960)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)

demandExp118 :: ConfigDemand
demandExp118 =
  ConfigDemand
    ("Uniform interarrival-time w/ Exp(118) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
        in generateOrdersFixedDueDateSlack sim (exponential (960 / 118)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)

demandExp135 :: ConfigDemand
demandExp135 =
  ConfigDemand
    ("Uniform interarrival-time w/ Exp(135) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
        in generateOrdersFixedDueDateSlack sim (exponential (960 / 135)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)


demandExp105 :: ConfigDemand
demandExp105 =
  ConfigDemand
    ("Uniform interarrival-time w/ Exp(105) with DDS=" <> tshow dueDateSlack)
    dds
    (\sim ->
       let pts = productTypes sim
           -- test :: IO ()
           -- test = do
           --   nrs <- foldM (\xs _ -> do
           --                  x <- generateOrdersFixedDueDateSlack sim (exponential (960 / 105)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack
           --                  return $ length x : xs
           --              ) [] [0..10000]
           --   putStrLn $ "res: " ++ show (fromIntegral (sum nrs) / fromIntegral (length nrs))
        in generateOrdersFixedDueDateSlack sim (exponential (960 / 105)) (uniformDistr 0.5001 (fromIntegral (length pts) + 0.4999)) dueDateSlack)


-- interArrivalTimeDistribution :: UniformDistribution
-- interArrivalTimeDistribution = uniformDistr (61.935483871/960) (274.285714286/960)

-- productTypeDistribution :: UniformDistribution
-- productTypeDistribution = uniformDistr 1 2

-- demandExponential

  -- generateOrdersFixedDueDateSlack sim interArrivalTimeDistribution productTypeDistribution dueDateSlack
