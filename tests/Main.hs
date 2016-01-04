module Main where

import Test.Tasty

import Tests.Parsers
import Tests.Picklers
import Tests.Stream

main :: IO ()
main = defaultMain $ testGroup "root" [ parserTests
                                      , picklerTests
                                      , streamTests
                                      ]
