{-# LANGUAGE BangPatterns #-}


module Releaser.Build
    ( buildBORL
    , buildSim
    , periodLength
    , ptTypes
    ) where

import           Control.DeepSeq                 (NFData, force)
import           Control.Lens                    (over)
import           Control.Monad
import           Data.Function                   (on)
import           Data.List                       (groupBy, nub, sort, sortBy)
import           Data.List                       (find, sortBy)
import qualified Data.Map                        as M
import           Data.Time.Clock
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.IO
import           System.Random                   (newStdGen)

import           ML.BORL
import           SimSim

import           Releaser.Action
import           Releaser.ActionFilter
import           Releaser.Demand
import           Releaser.Type

periodLength :: Time
periodLength = 1

buildSim :: IO SimSim
buildSim = newSimSimIO routing procTimes periodLength releaseImmediate dispatchFirstComeFirstServe shipOnDueDate

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))
                        ,(Product 2, fmap timeFromDouble . genContVar (uniformDistr (70/960) (130/960)))])
            ,(Machine 2,[(Product 1, fmap timeFromDouble . genContVar (uniformDistr (130/960) (170/960)))])
            ,(Machine 3,[(Product 2, fmap timeFromDouble . genContVar (uniformDistr (180/960) (200/960)))])
            ]

routing :: Routes
routing =
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 1   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 2, Machine 1) --> Queue 3
  , (Product 2, Queue 3)   --> Machine 3
  , (Product 2, Machine 3) --> FGI
  ]

ptTypes :: [ProductType]
ptTypes = sort $ nub $ map (fst . fst) routing


testDemand :: IO ()
testDemand = do
  let nr = 1000
  sim <- buildSim
  xs <- replicateM nr (generateOrders sim)
  let len = fromIntegral $ length (concat xs)
  putStr "Avg order slack time: "
  print $ timeToDouble (sum $ map orderSlackTime (concat xs)) / len
  putStr "Avg order arrival date: "
  print $ timeToDouble (sum $ map arrivalDate (concat xs)) / len
  putStr "Avg number of order per period: "
  print $ fromIntegral (sum (map length xs)) / fromIntegral nr
  putStr "Avg order due date: "
  print $ timeToDouble (sum $ map dueDate (concat xs)) / len
  putStr "Avg number of order per product type"
  print $ map length $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)
  print $ map (productType . head) $ groupBy ((==) `on` productType) $ sortBy (compare `on` productType) (concat xs)

------------------------------------------------------------
--------------------------- BORL ---------------------------
------------------------------------------------------------


actionConfig :: ActionConfig
actionConfig = ActionConfig
  { actLowerActionBound = -1
  , actUpperActionBound = 1
  , actPeriodLength     = periodLength
  , actProductTypes     = ptTypes
  }

actionFilterConfig :: ActionFilterPLTConfig
actionFilterConfig = ActionFilterPLTConfig
  { actFilMinimumPLT   = 1
  , actFilMaximumPLT   = 7
  , actFilPeriodLength = periodLength
  }

-- | BORL Parameters.
borlParams :: Parameters
borlParams = Parameters
  { _alpha            = 0.5
  , _beta             = 0.05
  , _delta            = 0.04
  , _gamma            = 0.30
  , _epsilon          = 0.075
  , _exploration      = 0.8
  , _learnRandomAbove = 0.0
  , _zeta             = 1.0
  , _xi               = 0.2
  }

-- | Decay function of parameters.
decay :: Decay
decay t  p@(Parameters alp bet del ga eps exp rand zeta xi)
  | t `mod` 200 == 0 =
    Parameters
      (max 0.03 $ slow * alp)
      (max 0.015 $ slow * bet)
      (max 0.015 $ slow * del)
      (max 0.01 $ slow * ga)
      (max 0.05 $ slow * eps) -- (0.5*bet)
      (max 0.10 $ slower * exp)
      rand
      zeta -- zeta
      -- (max 0.075 $ slower * xi)
      (0.5*bet)
  | otherwise = p
  where
    slower = 0.995
    slow = 0.98
    faster = 1.0 / 0.99
    f = max 0.01


buildBORL :: IO (BORL St)
buildBORL = do
  sim <- buildSim
  startOrds <- generateOrders sim
  let initSt = St sim startOrds RewardShippedSimple (M.fromList $ zip (productTypes sim) (map Time [1,1..]))
  let (actionList, actions) = mkConfig (actionsPLT initSt) actionConfig
  let actionFilter = mkConfig (actionFilterPLT actionList) actionFilterConfig
  let borl = mkUnichainTabular algBORL initSt id actions actionFilter borlParams decay Nothing
  askUser True usage cmds borl   -- maybe increase learning by setting estimate of rho
  return borl

  where cmds = []
        usage = []


askUser :: (NFData s, Ord s, Show s) => Bool -> [(String,String)] -> [(String, ActionIndexed s)] -> BORL s -> IO ()
askUser showHelp addUsage cmds ql = do
  let usage =
        sortBy (compare `on` fst) $
        [ ("v", "Print V+W tables")
        , ("p", "Print everything")
        , ("q", "Exit program (unsaved state will be lost)")
        , ("r", "Run for X times")
        , ("m", "Multiply all state values by X")
        -- , ("s" "Save to file save.dat (overwrites the file if it exists)")
        -- , ("l" "Load from file save.dat")
        , ("_", "Any other input starts another learning round\n")
        ] ++
        addUsage
  putStrLn ""
  when showHelp $ putStrLn $ unlines $ map (\(c, h) -> c ++ ": " ++ h) usage
  putStr "Enter value (h for help): " >> hFlush stdout
  c <- getLine
  case c of
    "h" -> askUser True addUsage cmds ql
    "?" -> askUser True addUsage cmds ql
    -- "s" -> do
    --   saveQL ql "save.dat"
    --   askUser ql addUsage cmds
    -- "l" -> do
    --   ql' <- loadQL ql "save.dat"
    --   print (prettyQLearner prettyState (text . show) ql')
    --   askUser ql addUsage cmds'
    "r" -> do
      putStr "How many learning rounds should I execute: " >> hFlush stdout
      l <- getLine
      case reads l :: [(Integer, String)] of
        [(nr, _)] -> mkTime (steps ql nr) >>= askUser False addUsage cmds
        _ -> do
          putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "p" -> do
      prettyBORL ql >>= print
      askUser False addUsage cmds ql
    "m" -> do
      putStr "Multiply by: " >> hFlush stdout
      l <- getLine
      case reads l :: [(Double, String)] of
        [(nr, _)] -> askUser False addUsage cmds (foldl (\q f -> over (proxies . f) (multiplyProxy nr) q) ql [psiV, v, w])
        _ -> do
          putStr "Could not read your input :( You are supposed to enter an Integer.\n"
          askUser False addUsage cmds ql
    "v" -> do
      runMonadBorl (restoreTensorflowModels ql >> prettyBORLTables True False False ql) >>= print
      askUser False addUsage cmds ql
    _ ->
      case find ((== c) . fst) cmds of
        Nothing ->
          unless
            (c == "q")
            (step ql >>= \x -> runMonadBorl (restoreTensorflowModels ql >> prettyBORLTables True False True x) >>= print >> return x >>= askUser False addUsage cmds)
        Just (_, cmd) -> runMonadBorl (restoreTensorflowModels ql >> stepExecute (ql, False, cmd) >>= saveTensorflowModels) >>= askUser False addUsage cmds


mkTime :: NFData t => IO t -> IO t
mkTime a = do
    start <- getCurrentTime
    !val <- force <$> a
    end   <- getCurrentTime
    putStrLn ("Computation Time: " ++ show (diffUTCTime end start))
    return val

