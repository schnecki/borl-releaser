{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict       #-}
module Main where

import           Control.DeepSeq             (NFData, force)
import           Control.Lens
import           Control.Monad               (foldM, unless, when)
import qualified Data.ByteString             as BS
import           Data.Function               (on)
import           Data.List                   (find, sortBy)
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
  runMonadBorlIO $ do
    -- borl <- liftSimple buildBORLGrenade
    borl <- liftSimple buildBORLTable
  -- runMonadBorlTF $ do
  --   borl <- buildBORLTensorflow
    askUser True usage cmds borl   -- maybe increase learning by setting estimate of rho
  where cmds = []
        usage = []


askUser :: (MonadBorl' m) => Bool -> [(String,String)] -> [(String, ActionIndexed St)] -> BORL St -> m ()
askUser showHelp addUsage cmds ql = do
  let usage =
        sortBy (compare `on` fst) $
        [ ("v", "Print V+W tables")
        , ("p", "Print everything")
        , ("q", "Exit program (unsaved state will be lost)")
        , ("r", "Run for X times")
        , ("m", "Multiply all state values by X")
        , ("s", "Save to file savedModel (overwrites the file if it exists)")
        , ("sim", "Print the simulation")
        , ("l", "Load from file savedModel")
        , ("_", "Any other input starts another learning round\n")
        ] ++
        addUsage
  liftSimple $ putStrLn ""
  when showHelp $ liftSimple $ putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) usage
  liftSimple $ putStr "Enter value (type help to display the usage information): " >> hFlush stdout
  c <- liftSimple getLine
  case c of
    "help" -> askUser True addUsage cmds ql
    "?" -> askUser True addUsage cmds ql
    "s" -> do
      ser <- toSerialisableWith serializeSt id ql
      liftSimple $ BS.writeFile "savedModel" (S.runPut $ S.put ser)
      askUser showHelp addUsage cmds ql
    "sim" -> do
      let sim = ql ^. s . simulation
      liftSimple $ putStrLn $ T.unpack $ prettySimSim sim
      askUser showHelp addUsage cmds ql
    "h" -> do
      liftSimple $ prettyBORLHead True ql >>= print
      askUser False addUsage cmds ql
    "l" -> do
      bs <- liftSimple $ BS.readFile "savedModel"
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
      liftSimple $ putStr "How many learning rounds should I execute: " >> hFlush stdout
      l <- liftSimple getLine
      case reads l :: [(Integer, String)] of
        [(nr, _)] -> mkTime (stepsM ql nr) >>= askUser False addUsage cmds
        _ -> do
          liftSimple $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "p" -> do
      let ppElems = mkPrettyPrintElems (ql ^. s)
      liftSimple $ prettyBORL (setPrettyPrintElems ppElems ql) >>= print
      askUser False addUsage cmds ql
    "m" -> do
      liftSimple $ putStr "Multiply by: " >> hFlush stdout
      l <- liftSimple getLine
      case reads l :: [(Double, String)] of
        [(nr, _)] -> askUser False addUsage cmds (foldl (\q f -> over (proxies . f) (multiplyProxy nr) q) ql [psiV, v, w])
        _ -> do
          liftSimple $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "v" -> do
      case find isTensorflow (allProxies $ ql ^. proxies) of
        Nothing -> liftSimple $ prettyBORLTables True False False ql >>= print
        Just _ -> liftTensorflow (prettyBORLTables True False False ql) >>= liftSimple . print
      askUser False addUsage cmds ql
    _ ->
      case find ((== c) . fst) cmds of
        Nothing ->
          unless
            (c == "q")
            (stepM ql >>= \ql' ->
               case find isTensorflow (allProxies $ ql' ^. proxies) of
                 Nothing -> prettyBORLHead True ql' >>= liftSimple . print >> askUser False addUsage cmds ql'
                 Just _ -> do
                   b <- liftTensorflow (prettyBORLHead True ql')
                   liftSimple $ print b
                   askUser False addUsage cmds ql')
        Just (_, cmd) ->
          case find isTensorflow (allProxies $ ql ^. proxies) of
            Nothing -> liftSimple $ stepExecute (ql, False, cmd) >>= askUser False addUsage cmds
            Just _ -> liftTensorflow (stepExecute (ql, False, cmd) >>= saveTensorflowModels) >>= askUser False addUsage cmds


mkTime :: (MonadBorl' m, NFData t) => m t -> m t
mkTime a = do
    start <- liftSimple getCurrentTime
    !val <- force <$> a
    end   <- liftSimple getCurrentTime
    liftSimple $ putStrLn ("Computation Time: " ++ show (diffUTCTime end start))
    return val

mkPrettyPrintElems :: St -> [[Double]]
mkPrettyPrintElems st = zipWith (++) plts (replicate (length plts) base)
  where
    base = drop (length productTypes) (netInp st)
    minVal = configActFilterMin actionFilterConfig
    maxVal = configActFilterMax actionFilterConfig
    actList = map (scaleValue (fromIntegral minVal, fromIntegral maxVal) . fromIntegral) [minVal .. maxVal]
    plts = [[x, y] | x <- actList, y <- actList]


  -- where
    -- len = length productTypes + 1 + length [configActFilterMin actionFilterConfig .. configActFilterMax actionFilterConfig] + 1 + length [-configActFilterMax actionFilterConfig .. 0]
    -- (lows, highs) = (replicate len (-1), replicate len 1)
    -- -- curInput = netInp $  St sim [] RewardPeriodEndSimple (M.fromList $ zip productTypes (map Time [1,1 ..]))
    -- vals = zipWith (\lo hi -> map rnd [lo,lo + (hi - lo) / 3 .. hi]) lows highs
    -- valsRev = zipWith (\lo hi -> map rnd [hi,hi - (hi - lo) / 3 .. lo]) lows highs
    -- rnd x = fromIntegral (round (100 * x)) / 100
    -- ppSts = take 300 (combinations vals) ++ take 300 (combinations valsRev)
    -- combinations :: [[a]] -> [[a]]
    -- combinations [] = []
    -- combinations [xs] = map return xs
    -- combinations (xs:xss) = concatMap (\x -> map (x :) ys) xs
    --   where
    --     ys = combinations xss
