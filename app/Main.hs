{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Control.DeepSeq             (NFData, force)
import           Control.Lens
import           Control.Monad               (foldM, unless, void, when)
import           Control.Monad.IO.Class
import qualified Data.ByteString             as BS
import           Data.Function               (on)
import           Data.List                   (find, genericLength, sortBy)
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              as S
import qualified Data.Text                   as T
import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import qualified Data.Vector.Storable        as V
import           Grenade
import           System.IO                   (hFlush, stdout)
import           Text.PrettyPrint

import           ML.BORL
import           ML.BORL.InftyVector
import           SimSim                      hiding (productTypes)

import           Experimenter                (Availability (..), endState,
                                              experimentNumber, experimentResults,
                                              experiments, loadStateAfterPreparation,
                                              loadStateAfterPreparation2, mkAvailable,
                                              preparationResults, repetitionNumber)
import           Releaser.Build
import           Releaser.Release.ReleasePlt
import           Releaser.Settings           hiding (actionFilter, featureExtractor)
import           Releaser.Type


main :: IO ()
main
 = do
  putStr "Load state from experiment [Leave empty to skip]: " >> hFlush stdout
  mExpNr <- getIOMWithDefault Nothing
  case mExpNr of
    Nothing ->
      runMonadBorlIO $ do
        borl <- liftIO buildBORLGrenade
        -- borl <- liftIO buildBORLTable
        askUser True usage cmds borl   -- maybe increase learning by setting estimate of rho
      -- runMonadBorlTF $ do
      --   borl <- buildBORLTensorflow
      --   askUser True usage cmds borl -- maybe increase learning by setting estimate of rho
    Just expNr -> do
      liftIO $ putStr "Experiment replication: [1]" >> hFlush stdout
      repNr <- liftIO $ getIOWithDefault 1
      dbSetting <- liftIO databaseSetting
      void $
        loadStateAfterPreparation2
          runMonadBorlIO
          dbSetting
          expSetting
          ()
          buildBORLGrenade
          expNr
          repNr
          (\borl0 -> do
             -- borl <- saveTensorflowModels borl0
             -- askUser True usage cmds borl
              askUser True usage cmds borl0
          )
  where
    cmds = []
    usage = []

askUser :: (MonadBorl' m) => Bool -> [(String,String)] -> [(String, ActionIndexed St)] -> BORL St -> m ()
askUser showHelp addUsage cmds ql = do
  let usage =
        sortBy (compare `on` fst) $
        [ ("l", "Load from file savedModel")
        , ("c", "Copy agent and run following commands in copy (return to original with q)")
        , ("p", "Print everything")
        , ("param", "Change parameters")
        , ("q", "Exit program (unsaved state will be lost)")
        , ("r", "Run for X times")
        , ("s", "Save to file savedModel (overwrites the file if it exists)")
        , ("sim", "Print the simulation")
        , ("v", "Print V+W tables")
        , ("_", "Any other input starts another learning round\n")
        ] ++
        addUsage
  liftIO $ putStrLn ""
  when showHelp $ liftIO $ putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) usage
  liftIO $ putStr "Enter value (type help to display the usage information): " >> hFlush stdout
  c <- liftIO getLine
  case c of
    "help" -> askUser True addUsage cmds ql
    "?" -> askUser True addUsage cmds ql
    "c" -> do
      liftIO $ putStrLn "\nINFO: Entering copyied agent environment. Return with quit to orignal one"
      askUser True addUsage cmds ql
      liftIO $ putStrLn "Returning from copy to original version"
      askUser True addUsage cmds ql
    "sim" -> do
      let sim = ql ^. s . simulation
      liftIO $ putStrLn $ T.unpack $ prettySimSim sim
      askUser showHelp addUsage cmds ql
    "h" -> do
      liftIO $ prettyBORLHead True (Just $ mInverse ql) ql >>= print
      askUser False addUsage cmds ql
    "param" -> do
      e <-
        liftIO $ do
          putStrLn "Which settings to change:"
          putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) $
            sortBy (compare `on` fst) [("alpha", "alpha"), ("exp", "exploration rate"), ("eps", "epsilon"), ("lr", "learning rate"), ("dislearn", "Disable/Enable all learning"), ("drop", "set the dropout active flag")]
          liftIO $ putStr "Enter value: " >> hFlush stdout >> getLine
      ql' <-
        case e of
          "drop" -> do
            liftIO $ putStr "Dropout active (True or False): " >> hFlush stdout
            liftIO $
              maybe
                ql
                (\(v' :: Bool) ->
                   overAllProxies
                     (filtered isGrenade)
                     (\(Grenade tar wor tp cfg act) -> Grenade (runSettingsUpdate (NetworkSettings v') tar) (runSettingsUpdate (NetworkSettings v') wor) tp cfg act)
                     ql) <$>
              getIOMWithDefault Nothing
          "alpha" -> do
            liftIO $ putStr "New value: " >> hFlush stdout
            liftIO $ maybe ql (\v' -> decaySetting . alpha .~ NoDecay $ parameters . alpha .~ v' $ ql) <$> getIOMWithDefault Nothing
          "exp" -> do
            liftIO $ putStr "New value: " >> hFlush stdout
            liftIO $ maybe ql (\v' -> decaySetting . exploration .~ NoDecay $ parameters . exploration .~ v' $ ql) <$> getIOMWithDefault Nothing
          "eps" -> do
            liftIO $ putStr "New value: " >> hFlush stdout
            liftIO $ maybe ql (\v' -> decaySetting . epsilon .~ Last NoDecay $ parameters . epsilon .~ Last (v' :: Float) $ ql) <$> getIOMWithDefault Nothing
          "lr" -> do
            liftIO $ putStr "New value: " >> hFlush stdout
            liftIO $
              maybe
                ql
                (\v' -> overAllProxies (proxyNNConfig . grenadeLearningParams) (setLearningRate v') $ overAllProxies (proxyNNConfig . learningParamsDecay) (const NoDecay) ql) <$>
              getIOMWithDefault Nothing
          "dislearn" -> do
            liftIO $ putStr "New value (True or False): " >> hFlush stdout
            liftIO $ maybe ql (\v' -> settings . disableAllLearning .~ v' $ ql) <$> getIOMWithDefault Nothing
          _ -> liftIO $ putStrLn "Did not understand the input" >> return ql
      askUser False addUsage cmds ql'
    "s" -> do
      ser <- toSerialisableWith serializeSt id ql
      liftIO $ BS.writeFile "savedModel" (S.runPut $ S.put ser)
      askUser showHelp addUsage cmds ql
    "l" -> do
      bs <- liftIO $ BS.readFile "savedModel"
      case S.runGet S.get bs of
        Left err -> do
          liftIO $ putStrLn err
          askUser showHelp addUsage cmds ql
        Right ser -> do
          let (St sim _ _ _) = ql ^. s
          let (_, actions) = mkConfig (action (ql ^. s)) actionConfig
          ql' <-
            fromSerialisableWith
              (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
              id
              actions
              (ql ^. actionFilter)
              netInp
              (modelBuilderTf actions (ql ^. s))
              ser
          askUser showHelp addUsage cmds ql'
    "r" -> do
      liftIO $ putStr "How many learning rounds should I execute: " >> hFlush stdout
      l <- liftIO getLine
      case reads l :: [(Integer, String)] of
        [(nr, _)] -> do
          liftIO $ putStr "How often shall I repeat this? [1] " >> hFlush stdout
          l <- liftIO getLine
          case reads l :: [(Integer, String)] of
            [(often, _)] -> do
              ql' <-
                foldM
                  (\q x -> do
                     !q' <- mkTime (stepsM q nr)
                     liftIO $ putStrLn $ "Steps done: " ++ show (nr * x) ++ "/" ++ show (nr * often)
                     let qPP = overAllProxies (proxyNNConfig . prettyPrintElems) (\pp -> pp ++ [(q' ^. featureExtractor) (ql ^. s), (q' ^. featureExtractor) (q' ^. s)]) q'
                     output <- prettyBORLMWithStInverse (Just $ mInverse ql) qPP
                     liftIO $ print output >> hFlush stdout
                     return $! force q')
                  ql
                  [1 .. often]
              askUser False addUsage cmds (force ql')
            _ -> stepsM ql nr >>= askUser False addUsage cmds
        _ -> do
          liftIO $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "p" -> do
      let ppElems = mkPrettyPrintElems True (ql ^. s)
          setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
      prettyBORLMWithStInverse (Just $ mInverse ql) (setPrettyPrintElems ql) >>= liftIO . print
      askUser False addUsage cmds ql
    "pp" -> do
      let ppElems = mkPrettyPrintElems False (ql ^. s)
          setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
      prettyBORLMWithStInverse (Just $ mInverse ql) (setPrettyPrintElems ql) >>= liftIO . print
      askUser False addUsage cmds ql
    "ps" -> do
      let ppElems = mkMiniPrettyPrintElems (ql ^. s)
          setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
      prettyBORLMWithStInverse (Just $ mInverse ql) (setPrettyPrintElems ql) >>= liftIO . print
      askUser False addUsage cmds ql
    "pm" -> do
      let ppElems = mkPrettyPrintElems True (ql ^. s)
          setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
      prettyBORLTables (Just $ mInverse ql) True False False (setPrettyPrintElems ql) >>= liftIO . print
      askUser False addUsage cmds ql
    "v" -> do
      case find isTensorflow (allProxies $ ql ^. proxies) of
        Nothing -> liftIO $ prettyBORLTables Nothing True False False ql >>= print
        Just _ -> liftTensorflow (prettyBORLTables Nothing True False False ql) >>= liftIO . print
      askUser False addUsage cmds ql
    _ ->
      case find ((== c) . fst) cmds of
        Nothing ->
          unless
            (c == "q")
            (stepM ql >>= \ql' -> do
               let ppElems = mkPrettyPrintElems True (ql' ^. s)
                   setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
               prettyBORLMWithStInverse (Just $ mInverse ql') (setPrettyPrintElems ql') >>= liftIO . print
               askUser False addUsage cmds ql')
        Just (_, cmd) ->
          case find isTensorflow (allProxies $ ql ^. proxies) of
            Nothing -> liftIO $ stepExecute ql ((False, cmd), []) >>= askUser False addUsage cmds
            Just _ -> liftTensorflow (stepExecute ql ((False, cmd), []) >>= saveTensorflowModels) >>= askUser False addUsage cmds


getIOWithDefault :: forall a . (Read a) => a -> IO a
getIOWithDefault def = do
  line <- getLine
  case reads line :: [(a,String)] of
    [(x,_)] -> return x
    _       -> return def

getIOMWithDefault :: forall m a . (Monad m, Read a) => m a -> IO (m a)
getIOMWithDefault def = do
  line <- getLine
  case reads line :: [(a, String)] of
    [(x, _)] -> return $ return x
    _        -> return def


mkTime :: (MonadBorl' m, NFData t) => m t -> m t
mkTime a = do
    start <- liftIO getCurrentTime
    !val <- force <$> a
    end   <- liftIO getCurrentTime
    liftIO $ putStrLn ("Computation Time: " ++ show (diffUTCTime end start))
    return val

mkPrettyPrintElems :: Bool -> St -> [V.Vector Float]
mkPrettyPrintElems usePlts st
  | usePlts = [netInp st]
  | otherwise = map V.fromList $ zipWith (++) plts (replicate (length plts) base)
  where
    base = drop (length productTypes) (V.toList $ netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleValue scaleAlg (Just (fromIntegral minVal, fromIntegral maxVal)) . fromIntegral) [minVal .. maxVal]
    plts = [[x, y] | x <- actList, y <- actList]

