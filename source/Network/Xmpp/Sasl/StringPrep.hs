{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Sasl.StringPrep where

import Text.StringPrep
import qualified Data.Set as Set
import Data.Text(singleton)

nonAsciiSpaces = Set.fromList [ '\x00A0', '\x1680', '\x2000', '\x2001', '\x2002'
                              , '\x2003', '\x2004', '\x2005', '\x2006', '\x2007'
                              , '\x2008', '\x2009', '\x200A', '\x200B', '\x202F'
                              , '\x205F', '\x3000'
                              ]

toSpace x = if x `Set.member` nonAsciiSpaces then " " else singleton x

saslPrepQuery = Profile
    [b1, toSpace]
    True
    [c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
    True

saslPrepStore = Profile
    [b1, toSpace]
    True
    [a1, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
    True

normalizePassword = runStringPrep saslPrepStore
normalizeUsername = runStringPrep saslPrepQuery