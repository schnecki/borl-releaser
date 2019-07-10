{-# LANGUAGE OverloadedStrings #-}

module Releaser.FeatureExtractor.Type
    ( ConfigFeatureExtractor (..)
    , Extraction (..)
    , extractionToList
    ) where


import           Data.Text     (Text)
import           Text.Printf

import           Releaser.Type


data Extraction = Extraction
  { extPlts      :: [Double]
  , extOrderPool :: [[Double]]
  , extQueues    :: [[[Double]]] -- ^ Queue, ProductType, Period
  -- , extMachines  :: [Double]
  , extFgi       :: [[Double]]  -- ^ ProductType, Period
  , extShipped   :: [[Double]]  -- ^ ProductType, Period
  }

instance Show Extraction where
  show (Extraction plts op que fgi shipped) =
    filter (\x -> x /= '"' && x /= '\\') $
    show $ map printFloat plts ++ map (show . map printFloat) op ++ map (show . map (map printFloat)) que ++ map (show . map printFloat) fgi ++ map (show . map printFloat) shipped
    where
      printFloat :: Double -> String
      printFloat = printf "%2.0f"


extractionToList :: Extraction -> [Double]
extractionToList (Extraction plts op que fgi shipped) = plts ++ concat op ++ concat (concat que) ++ concat fgi ++ concat shipped


data ConfigFeatureExtractor = ConfigFeatureExtractor
  { configFeatureExtractorName :: Text
  , configFeatureExtractor     :: St -> Extraction
  }
