{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Type
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , extractionToList
    , fromListToExtraction
    ) where


import           Data.List                     (foldl')
import           Data.Text                     (Text)
import           Text.Printf

import           ML.BORL                       hiding (FeatureExtractor)

import           Releaser.SettingsActionFilter
import           Releaser.Type

scalePltsMin :: Double
scalePltsMin = fromIntegral $ configActFilterMin actionFilterConfig

scalePltsMax :: Double
scalePltsMax = fromIntegral $ configActFilterMax actionFilterConfig

scaleOrderMin :: Double
scaleOrderMin = 0

scaleOrderMax :: Double
scaleOrderMax = 12


data Extraction = Extraction
  { extPlts      :: [Double]
  , extOrderPool :: [[Double]]
  , extQueues    :: [[[Double]]] -- ^ Queue, ProductType, Period
  -- , extMachines  :: [Double]
  , extFgi       :: [[Double]]  -- ^ ProductType, Period
  , extShipped   :: [[Double]]  -- ^ ProductType, Period
  , scaleValues  :: Bool
  }

instance Show Extraction where
  show (Extraction plts op que fgi shipped _) =
    filter (\x -> x /= '"' && x /= '\\') $
    show $ map printFloat plts ++ map (show . map printFloat) op ++ map (show . map (map printFloat)) que ++ map (show . map printFloat) fgi ++ map (show . map printFloat) shipped
    where
      printFloat :: Double -> String
      printFloat = printf "%2.0f"


extractionToList :: Extraction -> [Double]
extractionToList (Extraction plts op que fgi shipped scale) =
  map (scalePlts scale) plts ++ map (scaleOrder scale) (concat op ++ concat (concat que) ++ concat fgi ++ concat shipped)


scalePlts, scaleOrder :: Bool -> Double -> Double
scalePlts scale
  | scale = scaleValue (Just (scalePltsMin, scalePltsMax))
  | otherwise = id
scaleOrder scale
  | scale = scaleValue (Just (scaleOrderMin, scaleOrderMax))
  | otherwise = id


unscalePlts, unscaleOrder :: Bool -> Double -> Double
unscalePlts scale
  | scale = unscaleValue (Just (scalePltsMin, scalePltsMax))
  | otherwise = id
unscaleOrder scale
  | scale = unscaleValue (Just (scaleOrderMin, scaleOrderMax))
  | otherwise = id


fromListToExtraction :: St -> ConfigFeatureExtractor -> [Double] -> Extraction
fromListToExtraction st (ConfigFeatureExtractor _ extr) xs =
  Extraction
    (map (unscalePlts scale) $ take plts xs)
    (splitN opL1 $ take (opRoot * opL1) $ map (unscaleOrder scale) $ drop plts xs)
    (splitN queL1 $ splitN queL2 $ take (queL1 * queL2 * queRoot) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1) xs)
    (splitN fgiL1 $ take (fgiRoot * fgiL1) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1 + queL1 * queL2 * queRoot) xs)
    (splitN shipL1 $ take (shipRoot * shipL1) $ map (unscaleOrder scale) $ drop (plts + opRoot * opL1 + queL1 * queL2 * queRoot + fgiRoot * fgiL1) xs)
    scale
  where
    sample = extr st
    scale = scaleValues sample
    plts = length (extPlts sample)
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
  { configFeatureExtractorName :: Text
  , configFeatureExtractor     :: St -> Extraction
  }
