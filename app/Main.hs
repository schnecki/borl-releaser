module Main where

import           Control.Monad  (void)

import           Releaser.Build

main :: IO ()
main = void $ buildBORL
