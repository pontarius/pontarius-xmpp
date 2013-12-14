{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Network.Xmpp.Types

bench_jidFromTexts = whnf (\(a,b,c) -> jidFromTexts a b c)
                            ( Just "+\227\161[\\3\8260\&4"
                            , "\242|8e3\EOTrf6\DLEp\\\a"
                            , Just ")\211\226")

main = do defaultMain [bench "jidFromTexts 2" bench_jidFromTexts]
