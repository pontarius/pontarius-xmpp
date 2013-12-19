-- pilfered from lens package

module Main(main) where

import Build_doctest (deps)

import Control.Applicative
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import Test.DocTest

main :: IO ()
main = doctest $
    "-isource"
  : "-idist/build/autogen"
  : "-hide-all-packages"
  : "-XQuasiQuotes"
  : "-DWITH_TEMPLATE_HASKELL"
  : map ("-package="++) deps ++ sources

sources :: [String]
sources = ["source/Network/Xmpp/Types.hs"]

-- getSources :: IO [FilePath]
-- getSources = filter (isSuffixOf ".hs") <$> go "source"
--   where
--     go dir = do
--       (dirs, files) <- getFilesAndDirectories dir
--       (files ++) . concat <$> mapM go dirs

-- getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
-- getFilesAndDirectories dir = do
--   c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
--   (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
