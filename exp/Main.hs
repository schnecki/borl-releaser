{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where


import           Control.Lens
import           Control.Monad          (forM_, void)
import           Control.Monad.IO.Class
import qualified Data.Vector.Storable   as V
import           Prelude                hiding (scaleDouble)
import           System.Environment     (getArgs, getProgName)

import           Experimenter
import           ML.BORL                hiding (featureExtractor)

import           Releaser.Build
import           Releaser.Settings
import           Releaser.Type


main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      name <- getProgName
      putStrLn $ "Usage: " <> name <> " [run,eval,csv]"
    else forM_ args $ \case
             "eval" -- Generate results only
              ->
               loadAndEval liftIO liftIO buildBORLGrenade -- ANN version
             "run" ->
               -- run liftIO liftIO buildBORLTable   -- Lookup table version
               run liftIO liftIO buildBORLGrenade -- ANN version
             "csv" ->
               -- Generate CSV only
               loadAndWriteCsv liftIO buildBORLGrenade -- ANN version
             _ -> do
               putStrLn "Unkown command\n"
               name <- getProgName
               putStrLn $ "Usage: " <> name <> " [run,eval,csv]"


run ::
     (ExpM (BORL St Act) (Bool, Experiments (BORL St Act)) -> IO (Bool, Experiments (BORL St Act)))
  -> (ExpM (BORL St Act) (Evals (BORL St Act)) -> IO (Evals (BORL St Act)))
  -> ExpM (BORL St Act) (BORL St Act)
  -> IO ()
run runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  (changed, res) <- runExperimentsM runner dbSetting expSetting () mkInitSt
  liftIO $ putStrLn $ "Any change: " ++ show changed
  eval dbSetting runner2 res

loadAndEval ::
     (ExpM (BORL St Act) (Maybe (Experiments (BORL St Act))) -> IO (Maybe (Experiments (BORL St Act))))
  -> (ExpM (BORL St Act) (Evals (BORL St Act)) -> IO (Evals (BORL St Act)))
  -> ExpM (BORL St Act) (BORL St Act)
  -> IO ()
loadAndEval runner runner2 mkInitSt = do
  dbSetting <- databaseSetting
  Just res <- loadExperimentsResultsM True runner dbSetting expSetting () mkInitSt 1
  eval dbSetting runner2 res

loadAndWriteCsv :: (ExpM (BORL St Act) (Maybe (Experiments (BORL St Act))) -> IO (Maybe (Experiments (BORL St Act)))) -> ExpM (BORL St Act) (BORL St Act) -> IO ()
loadAndWriteCsv runner mkInitSt = do
  dbSetting <- databaseSetting
  Just res <- loadExperimentsResultsM False runner dbSetting expSetting () mkInitSt 1
  liftIO $ writeCsvMeasure dbSetting res (SmoothingMovAvg 300) ["AvgReward", "BOC", "WIPC", "FGIC", "SUMC"] -- , "VAvg", "PsiV", "PsiW"]


eval :: ExperimentDef a => DatabaseSetting -> (ExpM a (Evals a) -> IO (Evals a)) -> Experiments a -> IO ()
eval dbSetting runner2 res = do
  let evals = [ -- Sum OverPeriods $ Of "SUMC",
                -- Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC")
              -- , Sum OverPeriods $ Of "EARN", Mean OverReplications (Stats $ Sum OverPeriods $ Of "EARN")
              -- , Sum OverPeriods $ Of "BOC"
                Mean OverReplications (Stats $ Sum OverPeriods $ Of "WIPC")
              , Mean OverReplications (Stats $ Sum OverPeriods $ Of "FGIC")
              , Mean OverReplications (Stats $ Sum OverPeriods $ Of "BOC")
              -- , Name "FGIC+BOC" $ Id $ Stats (Mean OverReplications (Stats $ Sum OverPeriods $ Of "FGIC")) `Add` Stats (Mean OverReplications (Stats $ Sum OverPeriods $ Of "BOC"))
              , Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC")
              -- , Name "Costs per Period" $ Id $ Stats (Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC")) `Div` Length (Stats $ Mean OverReplications (Stats $ Sum OverPeriods $ Of "SUMC"))
              , Name "Average Reward" $ Mean OverReplications (Last $ Of "AvgReward")
              -- , Sum OverPeriods $ Of "WIPC"
              -- , Sum OverPeriods $ Of "FGIC"
              -- , Sum OverPeriods $ Of "SUMC"

              -- -- , Id $ EveryXthElem 4 $ Of "demand"
              -- -- , Id $ EveryXthElem 4 $ Of "op"
              -- -- , Id $ EveryXthElem 4 $ Of "wip"
              -- -- , Id $ EveryXthElem 4 $ Of "bo"
              -- -- , Id $ EveryXthElem 4 $ Of "fgi"

              , Name "SFTP" $ Mean OverReplications (Last $ Of "FTMeanFloor")
              , Name "SFTP StdDev" $ Mean OverReplications (Last $ Of "FTStdDevFloor") -- ,Id $ Last $ Of "FTStdDevFloor",
              , Name "SFTP+FGIT" $ Mean OverReplications (Last $ Of "FTMeanFloorAndFgi") -- , Id $ Last $ Of "FTMeanFloorAndFgi",
              , Name "SFTP+FGIT StdDev" $ Mean OverReplications (Last $ Of "FTStdDevFloorAndFgi") -- , Id $ Last $ Of "FTStdDevFloorAndFgi",
              , Name "Tardiness SF" $ Mean OverReplications (Last $ Of "TARDPctFloor")
              , Name "Tardiness SF+FGI" $ Mean OverReplications (Last $ Of "TARDPctFloorAndFgi") -- , Id $ Last $ Of "TARDMeanFloorAndFGI"
              -- , Mean OverReplications (Last $ Of "TARDMeanFloorAndFGI")
              -- -- , Id $ Last $ Of "TARDStdDevFloorAndFGI"
              -- , Mean OverReplications (Last $ Of "TARDStdDevFloorAndFGI")

              -- -- , Id $ Last $ Of "FTMeanFloor"
              -- -- , Id $ Last $ Of "FTStdDevFloor", Mean OverReplications (Last $ Of "FTStdDevFloor")
              -- -- , Id $ Last $ Of "TARDPctFloor", Mean OverReplications (Last $ Of "TARDPctFloor")
              -- -- , Id $ Last $ Of "TARDMeanFloor", Mean OverReplications (Last $ Of "TARDMeanFloor")
              -- -- , Id $ Last $ Of "TARDStdDevFloor", Mean OverReplications (Last $ Of "TARDStdDevFloor")

              -- -- , Id $ Last $ Of "AvgReward"
              -- , Mean OverReplications (Last $ Of "AvgReward")
              -- -- -- , Id $ EveryXthElem 10 $ Of "PLT P1", Mean OverReplications (EveryXthElem 10 $ Of "PLT P1")
              -- -- , Id $ EveryXthElem 4 $ Of "PLT P1" -- , Mean OverReplications (EveryXthElem 1 $ Of "PLT P1")
              -- -- -- , Id $ EveryXthElem 10 $ Of "PLT P2", Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")
              -- -- , Id $ EveryXthElem 4 $ Of "PLT P2" -- , Mean OverReplications (EveryXthElem 10 $ Of "PLT P2")

              -- -- , Id $ Last $ Of "PsiRho", Mean OverReplications (Last $ Of "PsiRho")
              -- -- , Id $ Last $ Of "PsiV", Mean OverReplications (Last $ Of "PsiV")
              -- -- , Id $ Last $ Of "PsiW", Mean OverReplications (Last $ Of "PsiW")


              , Sum OverPeriods $ Of "SUMC"
              ]
  evalRes <- genEvals runner2 dbSetting res evals
  print (view evalsResults evalRes)
  writeAndCompileLatex dbSetting evalRes

mkPrettyPrintElems :: St -> [V.Vector Double]
mkPrettyPrintElems st = map V.fromList $ zipWith (++) plts (replicate (length plts) base)
  where
    base = drop (length productTypes) (V.toList $ netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleDouble scaleAlg (Just (fromIntegral minVal, fromIntegral maxVal)) . fromIntegral) [minVal .. maxVal]
    plts = [[x, y] | x <- actList, y <- actList]
