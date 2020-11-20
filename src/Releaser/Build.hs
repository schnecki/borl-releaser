{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Unsafe              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Releaser.Build
    ( buildBORLTable
    , buildBORLGrenade
    , buildSim
    , borlSettings
    , scaleAlg
    , nnConfig
    , netInp
    , modelBuilder
    , actionConfig
    , experimentName
    , mInverse
    , databaseSetting
    , expSetting
    , mkMiniPrettyPrintElems
    ) where

import           Control.Arrow                     (second)
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Constraint                   (Dict (..))
import           Data.Default
import           Data.Int                          (Int64)
import           Data.List                         (find, genericLength)
import qualified Data.Map                          as M
import           Data.Maybe                        (fromMaybe, isJust)
import qualified Data.Proxy                        as Px
import           Data.Reflection                   (reifyNat)
import           Data.Serialize                    as S
import           Data.Serialize.Text               ()
import qualified Data.Text                         as T
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as E
import qualified Data.Vector.Storable              as V
import           GHC.Exts                          (IsList (..))
import           GHC.TypeLits                      (KnownNat)
import           Grenade
import           Network.HostName
import           Prelude
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Directory
import           System.Environment                (getArgs)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Unsafe.Coerce                     (unsafeCoerce)


-- ANN modules
import           Grenade

import           Experimenter                      hiding (sum)
import           ML.BORL                           as B hiding (actionFilter,
                                                         featureExtractor)
import qualified ML.BORL                           as B
import           SimSim                            hiding (productTypes)

import           Releaser.Costs.Type
import           Releaser.Decay.Type
import           Releaser.FeatureExtractor.Type
import           Releaser.Release.ReleasePlt
import           Releaser.Routing.Type
import           Releaser.SettingsAction
import           Releaser.SettingsActionFilter
import           Releaser.SettingsConfigParameters
import           Releaser.SettingsCosts
import           Releaser.SettingsDecay
import           Releaser.SettingsDemand
import           Releaser.SettingsFeatureExtractor
import           Releaser.SettingsPeriod
import           Releaser.SettingsReward
import           Releaser.SettingsRouting
import           Releaser.Type
import           Releaser.Util

import           Debug.Trace

buildSim :: IO SimSim
buildSim =
  newSimSimIO
    (configRoutingRoutes routing)
    -- procTimesConst
    procTimes
    periodLength
    -- (releaseBIL $ M.fromList [(Product 1, 1), (Product 2, 1)])
    -- (releaseBIL $ M.fromList [(Product 1, 2), (Product 2, 2)])
    -- (releaseBIL $ M.fromList [(Product 1, 3), (Product 2, 3)])
    -- (releaseBIL $ M.fromList [(Product 1, 4), (Product 2, 4), (Product 3, 4), (Product 4, 4), (Product 5, 4), (Product 6, 4)])
    -- (releaseBIL $ M.fromList (map (\pt -> (pt, 8)) productTypes))
    -- releaseImmediate
    (mkReleasePLT initialPLTS)
    dispatchFirstComeFirstServe
    shipOnDueDate

initialPLTS :: M.Map ProductType Time
initialPLTS = M.fromList $ zip productTypes [1 ..]


-- testDemand :: IO ()
-- testDemand = do
--   let nr = 1000
--   g <- createSystemRandom
--   sim <- buildSim
--   xs <- replicateM nr (generateOrders sim)
--   let len = fromIntegral $ length (concat xs)
--   putStr "Avg order slack time: "
--   print $ timeToDouble (sum $ map orderSlackTime (concat xs)) / len
--   putStr "Avg order arrival date: "
--   print $ timeToDouble (sum $ map arrivalDate (concat xs)) / len
--   putStr "Avg number of order per period: "
--   print $ fromIntegral (sum (map length xs)) / fromIntegral nr
--   putStr "Avg order due date: "
--   print $ timeToDouble (sum $ map dueDate (concat xs)) / len
--   putStr "Avg number of order per product type"
--   print $ map length $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)
--   print $ map (productType . head) $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)

------------------------------------------------------------
--------------------------- BORL ---------------------------
------------------------------------------------------------


instance Serialize Release where
  put (Release _ n) = S.put $ T.unpack n
  get = do
    n <- T.pack <$> S.get
    let fun | n == pltReleaseName = mkReleasePLT initialPLTS
            | n ==  uniqueReleaseName releaseImmediate = releaseImmediate
            | T.isPrefixOf bilName n = releaseBIL $ M.fromList bilArgs
              where ~bilArgs = read (T.unpack $ T.drop (T.length bilName) n)
                    ~bilName = T.takeWhile (/= '[') $ uniqueReleaseName (releaseBIL mempty)
    return fun


instance Show St where
  show st = show (extractFeatures False st)


netInp :: St -> V.Vector Double
netInp = extractionToList . extractFeatures True

mInverse :: BORL St Act -> NetInputWoAction -> Maybe (Either String St)
mInverse borl = return . Left . show . fromListToExtraction (borl ^. s) (featureExtractor True)

netInpTbl :: St -> V.Vector Double
netInpTbl st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> V.fromList $ plts ++ map reduce (concat $ op ++ map (map (fromIntegral . ceiling . (/9))) (concat que) ++ fgi ++ shipped)
  where
    reduce x = 7 * fromIntegral (ceiling (x / 7))

netInpTblBinary :: St -> [Double]
netInpTblBinary st = case extractFeatures False st of
  Extraction plts op que _ fgi shipped _ -> plts ++ map reduce (concat op) ++ map (fromIntegral . ceiling . (/9)) (concat (concat que)) ++ map reduce (concat $ fgi ++ shipped)
  where
    reduce x | x == 0 = x
             | otherwise = 1


modelBuilder :: St -> Integer -> IO SpecConcreteNetwork
modelBuilder initState cols =
  buildModelWith (NetworkInitSettings UniformInit BLAS Nothing) (DynamicBuildSetup False) $
  inputLayer1D lenIn >>
  -- fullyConnected (10*lenIn) >> dropout 0.99 >> leakyRelu >>
  -- fullyConnected (5*lenIn) >> leakyRelu >> dropout 0.95 >>
  -- fullyConnected (10*lenIn) >> leakyRelu >>
  -- fullyConnected (3*lenIn) >> leakyRelu >> dropout 0.98 >>
  -- fullyConnected (2*lenIn) >> leakyRelu >>
  -- fullyConnected lenIn >> leakyRelu >>
  -- fullyConnected (2*lenOut) >> leakyRelu >>
  -- fullyConnected lenOut >> reshape (lenActs, cols, 1) >> tanhLayer -- trivial
  -- fullyConnected (3*lenIn) >> relu >>
  -- fullyConnected (2 * lenIn) >> relu >>
  fullyConnected (2*lenIn) >> relu >>
  fullyConnected lenIn >> relu >>
  -- fullyConnected ((lenIn + lenOut) `div` 2) >> relu >>
  fullyConnected (2*lenOut) >> relu >>
  fullyConnected lenOut >> reshape (lenActs, cols, 1) >> tanhLayer
  where
    lenOut = lenActs * cols
    lenIn = fromIntegral $ V.length (netInp initState)
    lenActs = genericLength actions * fromIntegral (borlSettings ^. independentAgents)
    actions = [minBound .. maxBound] :: [Action Act]

--  *GOOD RESULTS* with DDS=12 and
-- SpecFullyConnected 45 90 :=> SpecRelu (90,1,1) :=> SpecFullyConnected 90 45 :=> SpecRelu (45,1,1) :=> SpecFullyConnected 45 6 :=> SpecRelu (6,1,1) :=> SpecFullyConnected 6 3 :=> SpecTanh (3,1,1) :=> SpecNNil1D 3


mkInitSt :: SimSim -> [Order] -> (AgentType -> IO St, St -> [V.Vector Bool])
mkInitSt sim startOrds =
  let initSt = St sim startOrds rewardFunction (M.fromList $ zip productTypes (repeat (Time 1)))
      actFilter = mkConfig actionFilter actionFilterConfig
  in (return . const initSt, actFilter)


buildBORLTable :: IO (BORL St Act)
buildBORLTable = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actFilter) = mkInitSt sim startOrds
  mkUnichainTabular alg initSt netInpTbl -- netInpTblBinary
    action actFilter borlParams (configDecay decay) borlSettings (Just initVals)

buildBORLGrenade :: IO (BORL St Act)
buildBORLGrenade = do
  sim <- buildSim
  startOrds <- liftIO $ generateOrders sim
  let (initSt, actFilter) = mkInitSt sim startOrds
  st <- liftIO $ initSt MainAgent
  flipObjective . setPrettyPrintElems <$> mkUnichainGrenadeCombinedNet alg initSt netInp action actFilter borlParams (configDecay decay) (modelBuilder st) nnConfig borlSettings (Just initVals)


setPrettyPrintElems :: BORL St Act -> BORL St Act
setPrettyPrintElems borl = setAllProxies (proxyNNConfig . prettyPrintElems) (ppElems borl) borl
  where ppElems borl = mkMiniPrettyPrintElems (borl ^. s)

copyFiles :: String -> ExperimentNumber -> RepetitionNumber -> Maybe ReplicationNumber -> IO ()
copyFiles pre expNr repetNr mRepliNr = do
  let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
  createDirectoryIfMissing True dir
  mapM_
    (\fn -> copyIfFileExists fn (dir <> pre <> fn <> "_exp_" <> show expNr <> "_rep_" <> show repetNr <> maybe "" (\x -> "_repl_" <> show x) mRepliNr))
    ["reward", "stateValues", "episodeLength", "plts", "costs", "stateVAllStates", "stateWAllStates", "statePsiVAllStates", "statePsiWAllStates"]

copyIfFileExists :: FilePath -> FilePath -> IO ()
copyIfFileExists fn target = do
  exists <- doesFileExist fn
  when exists $ copyFileWithMetadata fn target


databaseSetting :: IO DatabaseSetting
databaseSetting = do
  hostName <- getHostName
  args <- getArgs
  let mHostArg = case find ((== "--host=") . take 7) args of
                   x@Just{} -> drop 7 <$> x
                   Nothing  -> drop 3 <$> find ((== "-h=") . take 3) args
  let getPsqlHost h
        | isJust mHostArg = maybe "" (E.encodeUtf8 .T.pack) mHostArg
        | h `elem` ["schnecki-zenbook", "schnecki-laptop"] = "192.168.1.110"
        | otherwise = "c437-pc141"
  putStrLn $ "Using DB-Host: " <> show (getPsqlHost hostName)
  return $ DatabaseSetting ("host=" <> getPsqlHost hostName <> " dbname=experimenter user=experimenter password=experimenter port=5432") 10


mkMiniPrettyPrintElems :: St -> [V.Vector Double]
mkMiniPrettyPrintElems st
  | length (head xs) /= length base' = error $ "wrong length in mkMiniPrettyPrintElems: " ++
                                show (length $ head xs) ++ " instead of " ++ show (length base') ++ ". E.g.: " ++ show (map (unscaleDouble scaleAlg (Just (scaleOrderMin, scaleOrderMax))) base') ++
                                "\nCurrent state: " ++ show (extractFeatures True st)

  | otherwise = map V.fromList $ concatMap (zipWith (++) plts . replicate (length plts) . map (scaleDouble scaleAlg (Just (scaleOrderMin, scaleOrderMax)))) xs
  where
    len = V.length $ extractionToList $ extractFeatures True st
    base' = drop (length productTypes) (V.toList $ netInp st)

    plts :: [[Double]]
    plts = map (map (scaleDouble scaleAlg (Just (scalePltsMin, scalePltsMax))) . take (length productTypes)) [[1, 3, 1, 1, 3, 1], [3, 5, 3, 3, 5, 3]]
    xs :: [[Double]]
    xs | len - length (head plts) == 22 = [xsSimple, xsSimple2]
       | len - length (head plts) == 21 = map init [xsSimple, xsSimple2]
       | len - length (head plts) == 18 = [xs19, xs19']
       | len - length (head plts) == 35 = [xsFull1, xsFull2]
       | len - length (head plts) == 30 = [xs30', xs30]
       | len - length (head plts) == 29 = [xs29', xs29]
       | len - length (head plts) == 50 = [init xs51]
       | len - length (head plts) == 51 = [xs51]
       | len - length (head plts) == 45 = [xs45]
       | len - length (head plts) == 44 = [xs44, xs44']
       | len - length (head plts) == 106 = [xs106, xs106']
       | len - length (head plts) == 156 = [xs156, xs156']
       | len - length (head plts) == 226 = [xs226]
       | len - length (head plts) == 23 = [xs23]
       | len - length (head plts) == 534 = [xs534]
       | trace ("len - length (head plts): " ++ show (len - length (head plts)))len - length (head plts) == 678 = [xs684]
       | otherwise = error ("No mkMiniPrettyPrintElems in Build.hs setup for length: " ++ show len ++ "\nCurrent state: " ++ show (extractFeatures True st))
    xsSimple =              concat [concat [[ 0, 6, 8, 4, 4, 9, 9]], concat [ concat [[ 2]        ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0, 0, 5, 4]]]
                            -- concat [concat [[ 0, 6, 8, 4, 4, 9, 9]], concat [ concat [[ 2],  [2],[2]  ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 5, 4]]]
    xsSimple2 =             concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[23]         ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 3, 4, 9, 0]]
                                   -- concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[23],  [2],[12]  ]], concat [[ 1]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 9, 0]]
                                   ]
    xsFull1 = concat [ concat [[ 0, 6, 8, 4, 4, 9, 9]], concat  [concat [[ 2, 0, 0, 0, 0, 0, 0, 0]]], concat [[ 1, 0, 0, 0, 0, 0, 0, 0]], concat [[ 2, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0, 5, 4]]]
    xsFull2 = concat [ concat [[ 0, 0, 7, 10, 10, 9, 14]], concat  [concat [[ 2, 12, 9, 0, 0, 0, 0, 0]]], concat [[ 1, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 3, 4, 9, 0]]]
    xs51 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 0,13]], concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0]]]
    xs45 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 0,15]], concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]], concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0]]]
    xs30 = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 6, 0, 0, 0, 0, 0]]], concat [[]], concat [[ 1, 0, 0, 0, 0, 0, 0]], concat [[  0, 0, 0, 0]]]
    xs30' = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 6, 0, 0, 0, 0, 0]]], concat [[]], concat [[ 3, 2, 0, 0, 0, 0, 0]], concat [[ 3, 4, 9, 2]]]
    xs29 = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 6, 0, 0, 0, 0, 0]]], concat [[]], concat [[ 1, 0, 0, 0, 0, 0]], concat [[  0, 0, 0, 0]]]
    xs29' = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 6, 0, 0, 0, 0, 0]]], concat [[]], concat [[ 3, 2, 0, 0, 0, 0]], concat [[ 3, 4, 9, 2]]]
    xs19 = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 4]]], concat [[]], concat [[ 1, 0, 0, 0, 0, 0]], concat [[  0, 0, 0, 0]]]
    xs19' = concat [concat [[ 0, 0, 7,10,10, 9,14]], concat [ concat [[ 6 ]]], concat [[]], concat [[ 3, 2, 0, 0, 0, 0]], concat [[ 3, 4, 9, 2]]]

    xs44 = concat [ concat [[ 0, 0, 12, 14, 15, 11, 4, 10, 12, 8, 3,13]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[], concat [[ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 2]]]
    xs44' = concat [ concat [[ 0, 0, 1, 4, 5, 1, 4, 10, 12, 15, 12,13]], concat [ concat [[ 0, 0, 0, 0, 0, 4, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[], concat [[ 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 2]]]
    xs156 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8]],concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[], concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]
    xs156' = concat [concat [[ 0, 0, 4, 8, 5, 6, 5, 8, 8, 2, 3, 1],[ 0, 0, 0, 0, 0, 0, 0, 4, 7, 3, 5, 2]],concat [ concat [[ 0, 0, 7, 3, 3, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 7, 3, 3, 6, 5, 3, 2, 0, 0, 0, 0, 0, 0]]],[],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 1, 0, 0],[ 0, 0, 0, 5]]]

    xs106 = concat [ concat[[ 0, 0, 0, 0, 0, 0, 3],[ 0, 0, 0, 0, 0, 0, 7]],concat [ concat[[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[],concat [[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]
    xs106' = concat[ concat [[ 0, 0, 3, 9, 4, 6, 1],[ 0, 0, 0, 0, 0, 0, 2]],concat [ concat [[ 0, 0, 0, 0, 0, 4, 5, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0,11, 2, 0]],concat [[ 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0]]],[],concat [[ 2, 0, 0, 0, 0, 0],[ 5, 1, 8, 0, 0, 0]],concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]

    xs684 :: [Double]
    xs684 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 3],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]], concat[ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[], concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]
    xs226 :: [Double]
    xs226 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 5],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 3]], concat [ concat[[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]

    xs23 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 6]],concat[concat[[ 0]],concat [[ 0]],concat [[ 0]],concat [[ 0]],concat [[ 0]],concat [[ 0]]],[],concat [[ 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0]]]
    xs534 = concat [ concat [[ 0, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, 2],[ 0, 0, 0, 0, 0, 0, 3],[ 0, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, 0]], concat [concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]],[],concat [[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0],[ 0, 0, 0, 0, 0, 0]],concat [[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0],[ 0, 0, 0, 0]]]


------------------------------------------------------------
------------------ ExperimentDef instance ------------------
------------------------------------------------------------


instance ExperimentDef (BORL St Act) where
  -- type ExpM (BORL St Act) = TF.SessionT IO
  type ExpM (BORL St Act) = IO
  type Serializable (BORL St Act) = BORLSerialisable StSerialisable Act
  serialisable = do
    res <- toSerialisableWith serializeSt id
    return res
  deserialisable ser =
    unsafePerformIO $ do
      borl <- buildBORLGrenade
      let (St sim _ _ _) = borl ^. s
      -- let (_, actions) = mkConfig (action (borl ^. s)) actionConfig
      return $
        fromSerialisableWith
          (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
          id
          action
          (borl ^. B.actionFilter)
          netInp
          ser
  type InputValue (BORL St Act) = [Order]
  type InputState (BORL St Act) = ()
  -- ^ Generate some input values and possibly modify state. This function can be used to change the state. It is called
  -- before `runStep` and its output is used to call `runStep`.
  generateInput !_ !borl !_ !_ = do
    let !(St !_ !inc !_ !_) = borl ^. s
    return (inc, ())
  -- ^ Run a step of the environment and return new state and result.
  -- runStep :: (MonadIO m) => a -> InputValue a -> E.Period -> m ([StepResult], a)
  runStep !phase !borl !incOrds !_ = do
    !borl' <- stepM (set (s . nextIncomingOrders) incOrds borl)
    -- helpers
    when (borl ^. t `mod` 5000 == 0) $ liftIO $ prettyBORLHead True (Just $ mInverse borl) borl >>= print
    let !simT = timeToDouble $ simCurrentTime $ borl' ^. s . simulation
    let !borlT = borl' ^. t
    -- demand
    let !demand = StepResult "demand" (Just simT) (fromIntegral $ length $ borl ^. s . nextIncomingOrders)
    -- cost related measures
    let (StatsOrderCost !earnOld !wipOld !boOld !fgiOld) = simStatsOrderCosts $ simStatistics (borl ^. s . simulation)
    let (StatsOrderCost !earn !wip !bo !fgi) = simStatsOrderCosts $ simStatistics (borl' ^. s . simulation)
    let !cEarn  = StepResult "EARN" (Just simT) (fromIntegral (earn - earnOld))
    let !cBoc   = StepResult "BOC" (Just simT) (boCosts costConfig * fromIntegral (bo - boOld))
    let !cWip   = StepResult "WIPC" (Just simT) (wipCosts costConfig * fromIntegral (wip - wipOld))
    let !cFgi   = StepResult "FGIC" (Just simT) (fgiCosts costConfig * fromIntegral (fgi - fgiOld))
    let !cSum   = StepResult "SUMC" (Just simT) (cBoc ^. resultYValue + cWip ^. resultYValue + cFgi ^. resultYValue)
    let !curOp  = StepResult "op" (Just simT) (fromIntegral $ length $ simOrdersOrderPool $ borl' ^. s . simulation)
    let !curWip = StepResult "wip" (Just simT) (fromIntegral $ wip - wipOld)
    let !curBo  = StepResult "bo" (Just simT) (fromIntegral $ bo - boOld)
    let !curFgi = StepResult "fgi" (Just simT) (fromIntegral $ fgi - fgiOld)
    -- time related measures
    let (StatsFlowTime !ftNrFloorAndFgi !(StatsOrderTime !sumTimeFloorAndFgi !stdDevFloorAndFgi !_) !mTardFloorAndFgi) = simStatsShopFloorAndFgi $ simStatistics (borl' ^. s . simulation)
    let !tFtMeanFloorAndFgi     = StepResult "FTMeanFloorAndFgi" (Just simT) (fromRational sumTimeFloorAndFgi / fromIntegral ftNrFloorAndFgi)
    let !tFtStdDevFloorAndFgi   = StepResult "FTStdDevFloorAndFgi" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloorAndFgi)
    let !tTardPctFloorAndFgi    = StepResult "TARDPctFloorAndFgi" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloorAndFgi) mTardFloorAndFgi)
    let !tTardMeanFloorAndFgi   = StepResult "TARDMeanFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloorAndFgi)
    let !tTardStdDevFloorAndFgi = StepResult "TARDStdDevFloorAndFGI" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloorAndFgi)
    let !(StatsFlowTime !ftNrFloor !(StatsOrderTime !sumTimeFloor !stdDevFloor !_) !mTardFloor) = simStatsShopFloor $ simStatistics (borl' ^. s . simulation)
    let !tFtMeanFloor     = StepResult "FTMeanFloor" (Just simT) (fromRational sumTimeFloor / fromIntegral ftNrFloor)
    let !tFtStdDevFloor   = StepResult "FTStdDevFloor" (Just simT) (maybe 0 fromRational $ getWelfordStdDev stdDevFloor)
    let !tTardPctFloor    = StepResult "TARDPctFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromIntegral nrTard / fromIntegral ftNrFloor) mTardFloor)
    let !tTardMeanFloor   = StepResult "TARDMeanFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> fromRational sumTard / fromIntegral nrTard) mTardFloor)
    let !tTardStdDevFloor = StepResult "TARDStdDevFloor" (Just simT) (maybe 0 (\(StatsOrderTard nrTard sumTard stdDevTard) -> maybe 0 fromRational $ getWelfordStdDev stdDevTard) mTardFloor)
    -- BORL' related measures
    let valS !l = realToFrac (borl' ^?! l)
        valV !l = realToFrac $ head $ fromValue (borl' ^?! l)
        valL !l = realToFrac $ V.head $ borl' ^?! l
    let !avgRew    = StepResult "AvgReward" (Just $ fromIntegral borlT) (valL $ proxies . rho . proxyScalar)
        !expAvgRew = StepResult "ExpAvgReward" (Just $ fromIntegral borlT) (valS $ expSmoothedReward)
        !avgRewMin = StepResult "MinAvgReward" (Just $ fromIntegral borlT) (valL $ proxies . rhoMinimum . proxyScalar)
        !pltP1     = StepResult "PLT P1" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 1) (borl' ^. s . plannedLeadTimes))
        !pltP2     = StepResult "PLT P2" (Just $ fromIntegral borlT) (timeToDouble $ M.findWithDefault 0 (Product 2) (borl' ^. s . plannedLeadTimes))
        !psiRho    = StepResult "PsiRho" (Just $ fromIntegral borlT) (valV $ psis . _1)
        !psiV      = StepResult "PsiV" (Just $ fromIntegral borlT) (valV $ psis . _2)
        !psiW      = StepResult "PsiW" (Just $ fromIntegral borlT) (valV $ psis . _3)
        !vAvg      = StepResult "VAvg" (Just $ fromIntegral borlT) (avg $ V.toList $ borl' ^. lastRewards)
        !reward    = StepResult "Reward" (Just $ fromIntegral borlT) (realToFrac $ headWithDefault 0 $ borl' ^. lastRewards)
        avg !xs    = realToFrac $ sum xs / fromIntegral (length xs)
        headWithDefault d vec    = fromMaybe d $ vec V.!? 0
    return $! force $
      if phase /= EvaluationPhase
      then ([
          avgRew
        , avgRewMin
        , expAvgRew
        -- , vAvg
        , reward

        ], borl')

      else
      ( [-- cost related measures
          cSum
        , cEarn
        , cBoc
        , cWip
        , cFgi
             -- floor
        , curOp
        , curWip
        , curBo
        , curFgi
        , demand
             -- time related measures
        , tFtMeanFloorAndFgi
        , tFtStdDevFloorAndFgi
        , tTardPctFloorAndFgi
        , tTardMeanFloorAndFgi
        , tTardStdDevFloorAndFgi
        , tFtMeanFloor
        , tFtStdDevFloor
        , tTardPctFloor
        , tTardMeanFloor
        , tTardStdDevFloor
             -- BORL related measures
        , avgRew
        , avgRewMin
        , expAvgRew
        , pltP1
        , pltP2
        , psiRho
        , psiV
        , psiW
        , vAvg
        , reward
        ]
      , borl')
  -- ^ Provides the parameter setting.
  -- parameters :: a -> [ParameterSetup a]
  parameters borl =
    [ ParameterSetup
        "Algorithm"
        (set algorithm)
        (view algorithm)
        (Just $ return .
         const
           [ AlgDQNAvgRewAdjusted 0.75 1.0 ByStateValues
           ])
        Nothing
        Nothing
        Nothing
    , ParameterSetup
        "RewardType"
        (set (s . rewardFunctionOrders))
        (view (s . rewardFunctionOrders))
        (Just $ return .
         const
           [
            -- RewardPeriodEndSimple (ConfigRewardCosts (Just 500))
             RewardPeriodEndSimple (ConfigRewardCosts (Just 750))
           , RewardPeriodEndSimple (ConfigRewardCosts (Just 1250))
           ])
        Nothing
        Nothing
        Nothing
    , ParameterSetup
        "ReleaseAlgorithm"
        (\r -> over (s . simulation) (\sim -> sim {simRelease = r}))
        (simRelease . view (s . simulation))
        (Just $ return .
         const
           [ mkReleasePLT initialPLTS
           , releaseImmediate
           , releaseBIL (M.fromList [(Product 1, 6), (Product 2, 6)])
           , releaseBIL (M.fromList [(Product 1, 5), (Product 2, 5)])
           , releaseBIL (M.fromList [(Product 1, 4), (Product 2, 4)])
           , releaseBIL (M.fromList [(Product 1, 3), (Product 2, 3)])
           , releaseBIL (M.fromList [(Product 1, 2), (Product 2, 2)])
           , releaseBIL (M.fromList [(Product 1, 1), (Product 2, 1)])
           ])
        Nothing
        (Just (\x -> uniqueReleaseName x /= pltReleaseName)) -- drop preparation phase for all release algorithms but the BORL releaser
        (Just
           (\x ->
              if uniqueReleaseName x == pltReleaseName
                then FullFactory
                else SingleInstance -- only evaluate once if ImRe or BIL
            ))
    ] ++
    -- [ ParameterSetup
    --     "Learn Random Above until Exploration hits"
    --     (set (B.parameters . learnRandomAbove))
    --     (^. B.parameters . learnRandomAbove)
    --     (Just $ return . const [0.15])
    --     Nothing
    --     Nothing
    --     Nothing
    -- ] ++
    -- [ ParameterSetup
    --   "Xi (at period 0)"
    --   (set (B.parameters . xi))
    --   (^. B.parameters . xi)
    --   (Just $ return . const [5e-3])
    --   Nothing Nothing Nothing
    -- ] ++
    -- [ ParameterSetup
    --   "Zeta (at period 0)"
    --   (set (B.parameters . zeta))
    --   (^. B.parameters . zeta)
    --   (Just $ return . const [0.10])
    --   Nothing Nothing Nothing
    -- ] ++
    [ ParameterSetup
      "Alpha (at period 0)"
      (set (B.parameters . alpha))
      (^. B.parameters . alpha)
      (Just $ return . const [0.01])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Alpha"
      (set (B.decaySetting . alpha))
      (^. B.decaySetting . alpha)
      (Just $ return . const [ExponentialDecay (Just 1e-5) 0.55 30000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "AlphaRhoMin (at period 0)"
      (set (B.parameters . alphaRhoMin))
      (^. B.parameters . alphaRhoMin)
      (Just $ return . const [0.001])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay AlphaRhoMin"
      (set (B.decaySetting . alphaRhoMin))
      (^. B.decaySetting . alphaRhoMin)
      (Just $ return . const [ExponentialDecay (Just 1e-5) 0.55 30000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Delta (at period 0)"
      (set (B.parameters . delta))
      (^. B.parameters . delta)
      (Just $ return . const [0.005])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Delta"
      (set (B.decaySetting . delta))
      (^. B.decaySetting . delta)
      (Just $ return . const [ExponentialDecay (Just 5e-4) 0.55 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Gamma (at period 0)"
      (set (B.parameters . gamma))
      (^. B.parameters . gamma)
      (Just $ return . const [0.01])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Gamma"
      (set (B.decaySetting . gamma))
      (^. B.decaySetting . gamma)
      (Just $ return . const [ExponentialDecay (Just 1e-3) 0.55 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Epsilon (at period 0)"
      (set (B.parameters . epsilon))
      (^. B.parameters . epsilon)
      (Just $ return . const [fromList [0.30, 0.50], fromList [3.00, 0.50]])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Epsilon"
      (set (B.decaySetting . epsilon))
      (^. B.decaySetting . epsilon)
      (Just $ return . const [fromList [NoDecay]])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Exploration (at period 0)"
      (set (B.parameters . exploration))
      (^. B.parameters . exploration)
      (Just $ return . const [1.0])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Decay Exploration"
      (set (B.decaySetting . exploration))
      (^. B.decaySetting . exploration)
      (Just $ return . const [ExponentialDecay (Just 0.01) 0.55 50000])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Learn Random Above (faster converging rho)"
      (set (B.parameters . learnRandomAbove))
      (^. B.parameters . learnRandomAbove)
      (Just $ return . const [0.97])
      Nothing Nothing Nothing
    ] ++
    [ ParameterSetup
      "Replay Memory Size"
      (setAllProxies (proxyNNConfig . replayMemoryMaxSize))
      (^?! proxies . v . proxyNNConfig . replayMemoryMaxSize)
      (Just $ return . const [1000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Replay Memory Strategy"
      (setAllProxies (proxyNNConfig . replayMemoryStrategy))
      (^?! proxies . v . proxyNNConfig . replayMemoryStrategy)
      (Just $ return . const [ReplayMemoryPerAction, ReplayMemorySingle])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Training Batch Size"
      (setAllProxies (proxyNNConfig . trainBatchSize))
      (^?! proxies . v . proxyNNConfig . trainBatchSize)
      (Just $ return . const [4])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ANN (Grenade) Learning Rate"
      (setAllProxies (proxyNNConfig . grenadeLearningParams))
      (^?! proxies . v . proxyNNConfig . grenadeLearningParams)
      (Just $ return . const [OptAdam 0.005 0.9 0.999 1e-7 1e-3])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Grenade Smooth Target Update"
      (setAllProxies (proxyNNConfig . grenadeSmoothTargetUpdate))
      (^?! proxies . v . proxyNNConfig . grenadeSmoothTargetUpdate)
      (Just $ return . const [0.01])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Grenade Smooth Target Update Period"
      (setAllProxies (proxyNNConfig . grenadeSmoothTargetUpdatePeriod))
      (^?! proxies . v . proxyNNConfig . grenadeSmoothTargetUpdatePeriod)
      (Just $ return . const [100, 10])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ANN Learning Rate Decay"
      (setAllProxies (proxyNNConfig . learningParamsDecay))
      (^?! proxies . v . proxyNNConfig . learningParamsDecay)
      (Just $ return . const [ExponentialDecay (Just 5e-6) 0.55 100000])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "ScaleParameters"
      (setAllProxies (proxyNNConfig . scaleParameters))
      (^?! proxies . v . proxyNNConfig . scaleParameters)
      (Just $ return . const [ScalingNetOutParameters (-800) 800 (-5000) 5000 (-1500) 5000 (-5000) 5000
                             ,ScalingNetOutParameters (-800) 800 (-5000) 5000 (-1500) 3000 (-3000) 3000
                             ])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Scaling Out Setup"
      (setAllProxies (proxyNNConfig . scaleOutputAlgorithm))
      (^?! proxies . v . proxyNNConfig . scaleOutputAlgorithm)
      (Just $ return . const [ScaleMinMax])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Grenade Dropout Layers Flip-Active Periods"
      (setAllProxies (proxyNNConfig . grenadeDropoutFlipActivePeriod))
      (^?! proxies . v . proxyNNConfig . grenadeDropoutFlipActivePeriod)
      (Just $ return . const [10^5])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Grenade Dropout Only-Active After Period"
      (setAllProxies (proxyNNConfig . grenadeDropoutFlipActivePeriod))
      (^?! proxies . v . proxyNNConfig . grenadeDropoutFlipActivePeriod)
      (Just $ return . const [0])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "NStep"
      (set (settings . nStep))
      (^. settings . nStep)
      (Just $ return . const [3])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Exploration Strategy"
      (set (settings . explorationStrategy))
      (^. settings . explorationStrategy)
      (Just $ return . const [EpsilonGreedy])
      Nothing
      Nothing
      Nothing
    | isNN
    ] ++
    [ ParameterSetup
      "Workers Min Exploration"
      (set (settings . workersMinExploration))
      (^. settings . workersMinExploration)
      (Just $ return . const [replicate 10 0.01 ++ [0.05, 0.10, 0.20, 0.30]])
      Nothing
      Nothing
      Nothing
    | isNN
    ]
    where
      isNN = isNeuralNetwork (borl ^. proxies . v)
  -- HOOKS
  beforePreparationHook _ _ g borl =
    liftIO $ do
      let dir = "results/" <> T.unpack (T.replace " " "_" experimentName) <> "/data/"
      createDirectoryIfMissing True dir
      writeFile (dir ++ "plot.sh") gnuplot
      mapMOf (s . simulation) (setSimulationRandomGen g) borl
  beforeWarmUpHook _ _ _ g borl = liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0.00 $ set (B.settings . disableAllLearning) True borl
  beforeEvaluationHook _ _ _ g borl -- in case warm up phase is 0 periods
   = liftIO $ mapMOf (s . simulation) (setSimulationRandomGen g) $ set (B.parameters . exploration) 0.00 $ set (B.settings . disableAllLearning) True  borl
  afterPreparationHook _ expNr repetNr = liftIO $ copyFiles "prep_" expNr repetNr Nothing
  afterWarmUpHook _ expNr repetNr repliNr = liftIO $ copyFiles "warmup_" expNr repetNr (Just repliNr)
  afterEvaluationHook _ expNr repetNr repliNr = liftIO $ copyFiles "eval_" expNr repetNr (Just repliNr)


expSetting :: BORL St Act -> ExperimentSetting
expSetting borl =
  ExperimentSetting
    { _experimentBaseName = experimentName
    , _experimentInfoParameters = [actBounds, pltBounds, csts, dem, ftExtr, rout, dec, isNN]
    , _experimentRepetitions = 1
    , _preparationSteps = 1 * 10 ^ 6
    , _evaluationWarmUpSteps = 1000
    , _evaluationSteps = 100000
    , _evaluationReplications = 1
    , _maximumParallelEvaluations = 1
    }
  where
    isNNFlag = isNeuralNetwork (borl ^. proxies . v)
    isNN = ExperimentInfoParameter "Is Neural Network" isNNFlag
    dec = ExperimentInfoParameter "Decay" (configDecayName decay)
    actBounds = ExperimentInfoParameter "Action Bounds" (configActLower actionConfig, configActUpper actionConfig)
    pltBounds = ExperimentInfoParameter "Action Filter (Min/Max PLT)" (configActFilterMin actionFilterConfig, configActFilterMax actionFilterConfig)
    csts = ExperimentInfoParameter "Costs" costConfig
    dem = ExperimentInfoParameter "Demand" (configDemandName demand)
    ftExtr = ExperimentInfoParameter "Feature Extractor (State Representation)" (configFeatureExtractorName $ featureExtractor True)
    rout = ExperimentInfoParameter "Routing (Simulation Setup)" (configRoutingName routing)
