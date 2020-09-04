module Main where

import Control.Monad
import Data.Maybe
import Horrsubs.Lib
import System.Directory
import System.Environment

main :: IO ()
main = do
  dirPath <- extractDirPath =<< getArgs
  putStrLn $ "This is the given path: " <> dirPath
  paths <- fetchMkvPaths dirPath
  putStrLn "Target videos: "
  mapM_ (\path -> putStrLn path) paths
  videoProps <- catMaybes <$> mapM makeVideoProperty paths
  mapM_ extractSubtitles videoProps
  where
    extractDirPath :: [String] -> IO FilePath
    extractDirPath [] = getCurrentDirectory
    extractDirPath (x:_) = return x
