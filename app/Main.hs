{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict       #-}
module Main where

import           Control.DeepSeq     (NFData, force)
import           Control.Lens
import           Control.Monad       (foldM, unless, when)
import           Data.Function       (on)
import           Data.List           (find, sortBy)
import qualified Data.Text           as T
import           Data.Time.Clock     (diffUTCTime, getCurrentTime)
import           System.IO           (hFlush, stdout)
import           Text.PrettyPrint

import           ML.BORL
import           SimSim

import           Releaser.Build
import           Releaser.ReleasePLT
import           Releaser.Type


main :: IO ()
main = runMonadBorl $ do
  borl <- Simple buildBORLTable
  -- borl <- buildBORLTensorflow
  askUser True usage cmds borl   -- maybe increase learning by setting estimate of rho
  where cmds = []
        usage = []


askUser :: Bool -> [(String,String)] -> [(String, ActionIndexed St)] -> BORL St -> MonadBorl ()
askUser showHelp addUsage cmds ql = do
  let usage =
        sortBy (compare `on` fst) $
        [ ("v", "Print V+W tables")
        , ("p", "Print everything")
        , ("q", "Exit program (unsaved state will be lost)")
        , ("r", "Run for X times")
        , ("m", "Multiply all state values by X")
        , ("s", "Print simulation")
        -- , ("s" "Save to file save.dat (overwrites the file if it exists)")
        -- , ("l" "Load from file save.dat")
        , ("_", "Any other input starts another learning round\n")
        ] ++
        addUsage
  Simple $ putStrLn ""
  Simple $ when showHelp $ putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) usage
  Simple $ putStr "Enter value (h for help): " >> hFlush stdout
  c <- Simple getLine
  case c of
    "h" -> askUser True addUsage cmds ql
    "?" -> askUser True addUsage cmds ql
    "s" -> do
      let (St sim _ _ _) = ql ^. s
      Simple $ putStrLn (T.unpack $ prettySimSim sim) >> hFlush stdout
      askUser False addUsage cmds ql
    -- "s" -> do
    --   saveQL ql "save.dat"
    --   askUser ql addUsage cmds
    -- "l" -> do
    --   ql' <- loadQL ql "save.dat"
    --   print (prettyQLearner prettyState (text . show) ql')
    --   askUser ql addUsage cmds'
    "r" -> do
      Simple $ putStr "How many learning rounds should I execute: " >> hFlush stdout
      l <- Simple getLine
      case reads l :: [(Integer, String)] of
        [(nr, _)] -> mkTime (stepsM' ql nr) >>= askUser False addUsage cmds . force
          where stepsM' borl nr = do
                   !borl' <- foldM (\b _ -> stepsM ql nr >>= maybeDropTables) borl [1 .. min maxNr nr]
                   if nr > maxNr
                     then stepsM borl' (nr - maxNr)
                     else return borl'
                   where maxNr = 1000
                maybeDropTables borl = return $
                  if uniqueReleaseName (simRelease (borl^.s.simulation)) == pltReleaseName
                  then borl
                  else set (proxies.v.proxyTable) mempty $
                       set (proxies.w.proxyTable) mempty $
                       set (proxies.r0.proxyTable) mempty $
                       set (proxies.r1.proxyTable) mempty $
                       set (proxies.psiV.proxyTable) mempty borl


        _ -> do
          Simple $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "p" -> do
      Simple $ prettyBORL ql >>= print -- putStrLn . renderStyle wideStyle
      askUser False addUsage cmds ql
    "m" -> do
      Simple $ putStr "Multiply by: " >> hFlush stdout
      l <- Simple getLine
      case reads l :: [(Double, String)] of
        [(nr, _)] -> askUser False addUsage cmds (foldl (\q f -> over (proxies . f) (multiplyProxy nr) q) ql [psiV, v, w])
        _ -> do
          Simple $ putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "v" -> do
      -- restoreTensorflowModels ql >>
      prettyBORLTables True False False ql >>= Simple . putStrLn . renderStyle wideStyle
      askUser False addUsage cmds ql
    _ ->
      case find ((== c) . fst) cmds of
        Nothing ->
          unless
            (c == "q") $ do ql' <- stepM ql
                            prettyBORLTables True False True ql' >>= Simple . putStrLn . renderStyle wideStyle
                            askUser False addUsage cmds ql'
        Just (_, cmd) -> -- runMonadBorl (restoreTensorflowModels ql >>
          stepExecute (ql, False, cmd) >>=
          -- >>= saveTensorflowModels) >>=
          askUser False addUsage cmds


mkTime :: NFData t => MonadBorl t -> MonadBorl t
mkTime a = do
    start <- Simple getCurrentTime
    !val <- force <$> a
    end   <- Simple getCurrentTime
    Simple $ putStrLn ("Computation Time: " ++ show (diffUTCTime end start))
    return val

