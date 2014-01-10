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
  : "-itests"
  : "-idist/build/autogen"
  : "-hide-all-packages"
  : "-XQuasiQuotes"
  : "-XOverloadedStrings"
  : "-DWITH_TEMPLATE_HASKELL"
  : "-optP-includedist/build/autogen/cabal_macros.h"
  : map ("-package="++) deps
    ++ sources

sources :: [String]
sources = ["Network.Xmpp.Types"] -- ["source/Network/Xmpp/Types.hs"]

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "source"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
