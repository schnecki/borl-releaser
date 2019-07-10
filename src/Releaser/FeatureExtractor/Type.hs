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
  , extShipped   :: [[Double]]
  }

instance Show Extraction where
  show (Extraction plts op shipped) =
    filter (/= '"') $ show $ map printFloat plts ++
    map (show . map printFloat) op ++ map (show . map printFloat) shipped
    where
      printFloat :: Double -> String
      printFloat = printf "%2.0f"


extractionToList :: Extraction -> [Double]
extractionToList (Extraction plts op shipped) = plts ++ concat op ++ concat shipped


data ConfigFeatureExtractor = ConfigFeatureExtractor
  { configFeatureExtractorName :: Text
  , configFeatureExtractor     :: St -> Extraction
  }
