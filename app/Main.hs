module Main where

import Horrsubs.Lib
import System.Directory
import System.Environment

main :: IO ()
main = extractVideoSubtitles =<< extractDirPath =<< getArgs
  where
    extractDirPath :: [String] -> IO FilePath
    extractDirPath [] = getCurrentDirectory
    extractDirPath (x:_) = return x
