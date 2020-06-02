{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Type
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , extractionToList
    , fromListToExtraction
    , scalePltsMin
    , scalePltsMax
    , scaleOrderMin
    , scaleOrderMax
    ) where


import           Control.Concurrent.MVar
import           Data.List                         (foldl', genericLength)
import           Data.Text                         (Text)
import qualified Data.Vector.Storable              as V
import           System.IO.Unsafe
import           Text.Printf


import           ML.BORL                           hiding (FeatureExtractor)

import           Releaser.SettingsActionFilter
import           Releaser.SettingsConfigParameters
import           Releaser.SettingsRouting
import           Releaser.Type

scalePltsMin :: Float
scalePltsMin = fromIntegral $ configActFilterMin actionFilterConfig

scalePltsMax :: Float
scalePltsMax = fromIntegral $ configActFilterMax actionFilterConfig

scaleOrderMin :: Float
scaleOrderMin = 0

scaleOrderMax :: Float
scaleOrderMax = 5 -- 12


data Extraction = Extraction
  { extPlts      :: ![Float]
  , extOrderPool :: ![[Float]]
  , extQueues    :: ![[[Float]]] -- ^ Queue, ProductType, Period
  , extMachines  :: ![[Float]]   -- ^ Machine, Period
  , extFgi       :: ![[Float]]   -- ^ ProductType, Period
  , extShipped   :: ![[Float]]   -- ^ ProductType, Period
  , scaleValues  :: !Bool
  }

instance Show Extraction where
  show (Extraction plts op que mac fgi shipped _) =
    filter (\x -> x /= '"' && x /= '\\') $
    show $ [map printFloat plts] ++ [map (show . map printFloat) op] ++ [map (show . map (map printFloat)) que] ++ [map (show . map printFloat) mac] ++ [map (show . map printFloat) fgi] ++ [map (show . map printFloat) shipped]
    where
      printFloat :: Float -> String
      printFloat = printf "%2.0f"


extractionToList :: Extraction -> V.Vector Float
extractionToList (Extraction plts op que mac fgi shipped scale) =
  -- checkSize $
  V.fromList $ map (scalePlts scale) plts ++ map (scaleOrder scale) (concat op ++ concat (concat que)) ++ concat (scaleMachines scale mac) ++ map (scaleOrder scale) (concat fgi ++ concat shipped)
  where
    checkSize :: [Float] -> [Float]
    checkSize xs =
      unsafePerformIO $ do
        Just nr <- tryReadMVar cacheMVar
        if nr < 0
          then modifyMVar_ cacheMVar (const $ return $ length xs) >> return xs
          else if length xs /= nr
                 then error $ "the feature length changed: " ++ show xs
                 else return xs
    cacheMVar :: MVar Int
    cacheMVar = unsafePerformIO $ newMVar (-1)
    {-# NOINLINE cacheMVar #-}

scalePlts, scaleOrder :: Bool -> Float -> Float
scalePlts scale
  | scale = scaleValue scaleAlg (Just (scalePltsMin, scalePltsMax))
  | otherwise = id
scaleOrder scale
  | scale = scaleValue scaleAlg (Just (scaleOrderMin, scaleOrderMax))
  | otherwise = id

scaleMachines :: Bool -> [[Float]] -> [[Float]]
scaleMachines _ [] = []
scaleMachines True xs@[[_]] = map (map (scaleValue scaleAlg (Just (0, genericLength machines)))) xs
scaleMachines scale xs
  | scale = map (map (scaleValue scaleAlg (Just (0, 1)))) xs
  | otherwise = xs

unscalePlts, unscaleOrder :: Bool -> Float -> Float
unscalePlts scale
  | scale = unscaleValue scaleAlg (Just (scalePltsMin, scalePltsMax))
  | otherwise = id
unscaleOrder scale
  | scale = unscaleValue scaleAlg (Just (scaleOrderMin, scaleOrderMax))
  | otherwise = id
unscaleMachines :: Bool -> [[Float]] -> [[Float]]
unscaleMachines _ [] = []
unscaleMachines True xs@[[_]] = map (map (unscaleValue scaleAlg (Just (0, genericLength machines)))) xs
unscaleMachines scale xs
  | scale = map (map (unscaleValue scaleAlg (Just (0, 1)))) xs
  | otherwise = xs


fromListToExtraction :: St -> ConfigFeatureExtractor -> NetInputWoAction -> Extraction
fromListToExtraction st (ConfigFeatureExtractor _ extr) netInp =
  Extraction
    (map (unscalePlts scale) $ take plts xs)
    (splitN opL1 $ take (opRoot * opL1) $ map (unscaleOrder scale) $ drop plts xs)
    (splitN queL1 $ splitN queL2 $ take (queL1 * queL2 * queRoot) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1) xs)
    (unscaleMachines scale $ splitN machL1 $ take (macRoot * machL1) $ drop (plts + opRoot * opL1 + queL1 * queL2 * queRoot) xs)
    (splitN fgiL1 $ take (fgiRoot * fgiL1) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1 + queL1 * queL2 * queRoot + macRoot * machL1) xs)
    (splitN shipL1 $ take (shipRoot * shipL1) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1 + queL1 * queL2 * queRoot + macRoot * machL1 + fgiRoot * fgiL1) xs)
    scale
  where
    xs = V.toList netInp
    sample = extr st
    scale = scaleValues sample
    plts = length (extPlts sample)
    macRoot = length (extMachines sample)
    machL1
      | null (extMachines sample) = 0
      | otherwise = length (head $ extMachines sample)
    opRoot = length (extOrderPool sample)
    opL1
      | null (extOrderPool sample) = 0
      | otherwise = length (head $ extOrderPool sample)
    queRoot = length (extQueues sample)
    queL1
      | null (extQueues sample) = 0
      | otherwise = length (head $ extQueues sample)
    queL2
      | null (extQueues sample) = 0
      | null (head $ extQueues sample) = 0
      | otherwise = length (head $ head $ extQueues sample)
    fgiRoot = length (extFgi sample)
    fgiL1
      | null (extFgi sample) = 0
      | otherwise = length (head $ extFgi sample)
    shipRoot = length (extShipped sample)
    shipL1
      | null (extShipped sample) = 0
      | otherwise = length (head $ extShipped sample)
    splitN _ []  = []
    splitN nr xs = take nr xs : splitN nr (drop nr xs)


data ConfigFeatureExtractor = ConfigFeatureExtractor
  { configFeatureExtractorName :: !Text
  , configFeatureExtractor     :: !(St -> Extraction)
  }
