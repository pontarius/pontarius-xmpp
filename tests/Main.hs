module Main where

import Test.Tasty

import Tests.Parsers
import Tests.Picklers

main :: IO ()
main = defaultMain $ testGroup "root" [ parserTests
                                      , picklerTests
                                      ]
