module Main where

import Test.Tasty.HUnit
import Test.Tasty

import qualified Run.SendReceive as SendReceive
import qualified Run.Google as Google

sendReceiveTest = testCase "send and receive" SendReceive.run
googleTest = testCase "connect to google service" Google.connectGoogle

main = defaultMain $ testGroup "connection tests" [ sendReceiveTest
                                                  , googleTest
                                                  ]
