{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import Control.Monad (filterM)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Test.DocTest (doctest)

main :: IO ()
main = 
  getArgs >>= \case
    [] -> doctest =<< getSources
    xs -> doctest xs

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "src"
  where go dir = do
          (dirs, files) <- getFilesAndDirectories dir
          (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c

