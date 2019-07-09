{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where


import           Experimenter
import           ML.BORL

import           Network.HostName
import           Releaser.Build

databaseSetup :: IO DatabaseSetup
databaseSetup = do
  hostName <- liftIO getHostName
  case hostName of
    "schnecki-zenbook" -> DatabaseSetup "host=localhost dbname=experimenter user=schnecki password= port=5432" 10
    _ -> DatabaseSetup "host=c437-pc141 dbname=experimenter user=experimenter password=experimenter port=5432" 10


main :: IO ()
main = do

  -- run runMonadBorlIO runMonadBorlIO buildBORLTable   -- Lookup table version
  run runMonadBorlTF runMonadBorlTF buildBORLTensorflow -- ANN version

run :: (ExperimentDef a, InputState a ~ ()) => (ExpM a (Bool, Experiments a) -> IO (Bool, Experiments a)) -> (ExpM a (Experiments a) -> IO (Experiments a)) -> ExpM a a -> IO ()
run runner runner2 mkInitSt = do
  dbSetup <- databaseSetup
  (changed, res) <- runExperimentsM runner dbSetup expSetup () mkInitSt
  liftSimple $ putStrLn $ "Any change: " ++ show changed
  let evals = [ Sum OverPeriods $ Of "EARN", Mean OverReplications (Stats $ Sum OverPeriods $ Of "EARN")
              , Sum OverPeriods $ Of "BOC" , Mean OverReplications (Stats $ Sum OverPeriods $ Of "BOC")
              , Sum OverPeriods $ Of "WIPC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "WIPC")
              , Sum OverPeriods $ Of "FGIC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "FGIC")
              , Sum OverPeriods $ Of "SUMC", Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC")

              -- , Id $ EveryXthElem 4 $ Of "demand"
              -- , Id $ EveryXthElem 4 $ Of "op"
              -- , Id $ EveryXthElem 4 $ Of "wip"
              -- , Id $ EveryXthElem 4 $ Of "bo"
              -- , Id $ EveryXthElem 4 $ Of "fgi"

              -- , Id $ Last $ Of "FTMeanFloorAndFgi", Mean OverReplications (Last $ Of "FTMeanFloorAndFgi")
              -- , Id $ Last $ Of "FTStdDevFloorAndFgi", Mean OverReplications (Last $ Of "FTStdDevFloorAndFgi")
              -- , Id $ Last $ Of "TARDPctFloorAndFgi", Mean OverReplications (Last $ Of "TARDPctFloorAndFgi")
              -- , Id $ Last $ Of "TARDMeanFloorAndFGI", Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- , Id $ Last $ Of "TARDStdDevFloorAndFGI", Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- , Id $ Last $ Of "FTMeanFloor", Mean OverReplications (Last $ Of "FTMeanFloor")
              -- , Id $ Last $ Of "FTStdDevFloor", Mean OverReplications (Last $ Of "FTStdDevFloor")
              -- , Id $ Last $ Of "TARDPctFloor", Mean OverReplications (Last $ Of "TARDPctFloor")
              -- , Id $ Last $ Of "TARDMeanFloorAndFGI", Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- , Id $ Last $ Of "TARDStdDevFloorAndFGI", Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- , Id $ Last $ Of "AvgReward", Mean OverReplications (Last $ Of "AvgReward")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P1", Mean OverReplications (EveryXthElem 10 $ Of "PLT P1")
              -- , Id $ EveryXthElem 4 $ Of "PLT P1" -- , Mean OverReplications (EveryXthElem 1 $ Of "PLT P1")
              -- -- , Id $ EveryXthElem 10 $ Of "PLT P2", Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")
              -- , Id $ EveryXthElem 4 $ Of "PLT P2" -- , Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")

              , Id $ Last $ Of "PsiRho", Mean OverReplications (Last $ Of "PsiRho")
              , Id $ Last $ Of "PsiV", Mean OverReplications (Last $ Of "PsiV")
              , Id $ Last $ Of "PsiW", Mean OverReplications (Last $ Of "PsiW")
              ]
  evalRes <- genEvals runner2 databaseSetup res evals
  -- print (view evalsResults evalRes)
  writeAndCompileLatex evalRes

