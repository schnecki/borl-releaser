{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
module Main where

import           Control.DeepSeq             (NFData, force)
import           Control.Lens
import           Control.Monad               (foldM, unless, when)
import           Control.Monad.IO.Class
import qualified Data.ByteString             as BS
import           Data.Function               (on)
import           Data.List                   (find, genericLength, sortBy)
import           Data.Serialize              as S
import qualified Data.Text                   as T
import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           System.IO                   (hFlush, stdout)
import           Text.PrettyPrint

import           ML.BORL
import           SimSim                      hiding (productTypes)

import           Releaser.Build
import           Releaser.Release.ReleasePlt
import           Releaser.Settings           hiding (actionFilter)
import           Releaser.Type


main :: IO ()
main =
  -- runMonadBorlIO $ do
  --   -- borl <- liftIO buildBORLGrenade
  --   borl <- liftIO buildBORLTable
  --   askUser True usage cmds borl   -- maybe increase learning by setting estimate of rho


  runMonadBorlTF $ do
    borl <- buildBORLTensorflow
    let ppElems = mkMiniPrettyPrintElems (borl ^. s)
        setPrettyPrintElems = setAllProxies (proxyNNConfig.prettyPrintElems) ppElems
    askUser True usage cmds (setPrettyPrintElems borl)   -- maybe increase learning by setting estimate of rho
  where cmds = []
        usage = []


askUser :: (MonadBorl' m) => Bool -> [(String,String)] -> [(String, ActionIndexed St)] -> BORL St -> m ()
askUser showHelp addUsage cmds ql = do
  let usage =
        sortBy (compare `on` fst) $
        [ ("l", "Load from file savedModel")
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
    "s" -> do
      ser <- toSerialisableWith serializeSt id ql
      liftIO $ BS.writeFile "savedModel" (S.runPut $ S.put ser)
      askUser showHelp addUsage cmds ql
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
          putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) [("exp", "exploration rate"), ("eps", "epsilon")]
          liftIO $ putStr "Enter value: " >> hFlush stdout >> getLine
      ql' <- do
        let modifyDecayFun f v' = decayFunction .~ (\t p -> f .~ v' $ (ql ^. decayFunction) t p)
        case e of
             "exp" -> do
               liftIO $ putStr "New value: " >> hFlush stdout
               liftIO $ maybe ql (\v' -> modifyDecayFun exploration v' $ parameters . exploration .~ v' $ ql) <$> getIOWithDefault Nothing
             "eps" -> do
               liftIO $ putStr "New value: " >> hFlush stdout
               liftIO $ maybe ql (\v' -> modifyDecayFun epsilon v' $ parameters . epsilon .~ v' $ ql) <$> getIOWithDefault Nothing
             _ -> liftIO $ putStrLn "Did not understand the input" >> return ql
      askUser False addUsage cmds ql'
    "l" -> do
      bs <- liftIO $ BS.readFile "savedModel"
      case S.runGet S.get bs of
        Left err -> error err
        Right ser -> do
          borl <- liftTensorflow buildBORLTensorflow
          let (St sim _ _ _) = borl ^. s
          let (_, actions) = mkConfig (action (borl ^. s)) actionConfig
          ql' <-
            fromSerialisableWith
              (deserializeSt (simRelease sim) (simDispatch sim) (simShipment sim) (simProcessingTimes $ simInternal sim))
              id
              actions
              (borl ^. actionFilter)
              (borl ^. decayFunction)
              netInp
              netInp
              (modelBuilder actions (borl ^. s))
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
                  (\q _ -> do
                     !q' <- mkTime (stepsM q nr)
                     output <- prettyBORLMWithStateInverse (Just $ mInverse ql) q'
                     liftIO $ print output >> hFlush stdout
                     return $! force q')
                  ql
                  [1 .. often]
              askUser False addUsage cmds ql'
            _ -> stepsM ql nr >>= askUser False addUsage cmds
        _ -> do
          liftIO $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
      -- liftIO $ putStr "How many learning rounds should I execute: " >> hFlush stdout
      -- l <- liftIO getLine
      -- case reads l :: [(Integer, String)] of
      --   [(nr, _)] -> mkTime (stepsM ql nr) >>= askUser False addUsage cmds
      --   _ -> do
      --     liftIO $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
      --     askUser False addUsage cmds ql
    "p" -> do
      let ppElems = mkPrettyPrintElems (ql ^. s)
          setPrettyPrintElems = setAllProxies (proxyNNConfig . prettyPrintElems) ppElems
      liftIO $ prettyBORLWithStInverse (Just $ mInverse ql) (setPrettyPrintElems ql) >>= print
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
            (stepM ql >>= \ql' ->
               case find isTensorflow (allProxies $ ql' ^. proxies) of
                 Nothing -> prettyBORLHead True Nothing ql' >>= liftIO . print >> askUser False addUsage cmds ql'
                 Just _ -> do
                   b <- liftTensorflow (prettyBORLHead True Nothing ql')
                   liftIO $ print b
                   askUser False addUsage cmds ql')
        Just (_, cmd) ->
          case find isTensorflow (allProxies $ ql ^. proxies) of
            Nothing -> liftIO $ stepExecute (ql, False, cmd) >>= askUser False addUsage cmds
            Just _ -> liftTensorflow (stepExecute (ql, False, cmd) >>= saveTensorflowModels) >>= askUser False addUsage cmds


getIOWithDefault ::
     forall m a. (Monad m, Read a)
  => m a
  -> IO (m a)
getIOWithDefault def = do
  line <- getLine
  case reads line :: [(a,String)] of
    [(x,_)] -> return $ return x
    _       -> return def


mkTime :: (MonadBorl' m, NFData t) => m t -> m t
mkTime a = do
    start <- liftIO getCurrentTime
    !val <- force <$> a
    end   <- liftIO getCurrentTime
    liftIO $ putStrLn ("Computation Time: " ++ show (diffUTCTime end start))
    return val

mkPrettyPrintElems :: St -> [[Double]]
mkPrettyPrintElems st = zipWith (++) plts (replicate (length plts) base)
  where
    base = drop (length productTypes) (netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleValue (Just (fromIntegral minVal, fromIntegral maxVal)) . fromIntegral) [minVal .. maxVal]
    plts = [[x, y] | x <- actList, y <- actList]

mkMiniPrettyPrintElems :: St -> [[Double]]
mkMiniPrettyPrintElems st
  | length xs /= length base' =
    error $
    "wrong length in mkMiniPrettyPrintElems: " ++
    show (length xs) ++ " instead of " ++ show (length base') ++ ". E.g.: " ++ show (map (scaleValue (Just (scaleOrderMin, scaleOrderMax))) base')
  | otherwise = zipWith (++) plts (replicate (length plts) (map (scaleValue (Just (scaleOrderMin, scaleOrderMax))) xs))
  where
    base' = drop (length productTypes) (netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleValue (Just (scalePltsMin, scalePltsMax)) . fromIntegral) [minVal, minVal + maxVal `div` 2]
    plts = map (scaleValue (Just (scalePltsMin, scalePltsMax))) [1, 2] : [[x, y] | x <- actList, y <- actList, x == y]
    -- xs = [0, 0, 0, 4, 9, 9, 9] ++ concat ([[16]] ++ [[6]] ++ [[0]])
    --   ++ [2 / genericLength machines * scaleOrderMax]
    --   ++ [5, 3, 0, 0, 0, 0] ++ [0, 0, 0] :: [Double]
    xs = xsFull
    xsFull =
      [ -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -0.16666666666666663
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -0.33333333333333337
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      , -1.0
      ]
