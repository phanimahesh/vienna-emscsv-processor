module Main where

import Data.Default (def)
import System.Envy

import Lib
import Config
import RIO

main :: IO ()
main = do
  env <- decodeWithDefaults defConfig
  runRIO env processAllEntries
