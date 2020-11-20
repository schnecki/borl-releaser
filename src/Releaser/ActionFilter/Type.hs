{-# LANGUAGE BangPatterns #-}

module Releaser.ActionFilter.Type
    ( ActionFilterConfig (..)
    ) where


data ActionFilterConfig = ActionFilterConfig
  { configActFilterMin :: !Integer
  , configActFilterMax :: !Integer
  }
