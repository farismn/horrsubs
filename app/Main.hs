module Main where

import Data.Maybe
import Horrsubs.Lib
import System.Directory

main :: IO ()
main = do
  paths <- fetchMkvPaths =<< getLine
  videoProps <- catMaybes <$> mapM makeVideoProperty paths
  mapM_ extractSubtitles videoProps
