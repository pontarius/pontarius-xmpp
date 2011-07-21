{-

Copyright © 2010-2011 Jon Kristensen.

This file is part of Pontarius XMPP.

Pontarius XMPP is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Pontarius XMPP is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with Pontarius XMPP. If not, see <http://www.gnu.org/licenses/>.

-}


{-# OPTIONS_HADDOCK hide #-}

-- TODO: Make it possible to include host.
-- TODO: Host is assumed to be ISO 8859-1; make list of assumptions.
-- TODO: Can it contain newline characters?

module Network.XMPP.SASL (replyToChallenge) where

import Prelude hiding (concat, zipWith)
import Data.ByteString.Internal (c2w)
import Data.Char (isLatin1)
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as DBL (ByteString, append, pack,
                                              fromChunks, toChunks, null)
import qualified Data.ByteString.Lazy.Char8 as DBLC (append, pack, unpack)
import qualified Data.List as DL
import Data.Text (empty, singleton)
import Text.StringPrep (StringPrepProfile (..), a1, b1, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9, runStringPrep)
import Data.Ranges (inRanges, ranges)

import Crypto.HMAC (MacKey (MacKey), hmac)
import Crypto.Hash.SHA1 (SHA1)
import Data.Bits (xor)
import Data.ByteString ()
import Data.ByteString.Lazy (ByteString, concat, pack, toChunks, zipWith)
import Data.Serialize (Serialize, encodeLazy)
import Data.Serialize.Put (putWord32be, runPutLazy)

data Challenge1Error = C1MultipleCriticalAttributes       |
                       C1NotAllParametersPresent          |
                       C1SomeParamtersPresentMoreThanOnce |
                       C1WrongRealm                       |
                       C1UnsupportedAlgorithm             |
                       C1UnsupportedCharset               |
                       C1UnsupportedQOP
                       deriving Show


-- Will produce a list of key-value pairs given a string in the format of
-- realm="somerealm",nonce="OA6MG9tEQGm2hh",qop="auth",charset=utf-8...
stringToList :: String -> [(String, String)]
stringToList "" = []
stringToList s' = let (next, rest) = break' s' ','
                  in break' next '=' : stringToList rest
  where
    -- Like break, but will remove the first char of the continuation, if
    -- present.
    break' :: String -> Char -> (String, String)
    break' s' c = let (first, second) = break ((==) c) s'
                  in (first, removeCharIfPresent second c)

    -- Removes the first character, if present; "=hello" with '=' becomes
    -- "hello".
    removeCharIfPresent :: String -> Char -> String
    removeCharIfPresent [] _               = []
    removeCharIfPresent (c:t) c' | c == c' = t
    removeCharIfPresent s' c               = s'

-- Counts the number of directives in the pair list.
countDirectives :: String -> [(String, String)] -> Int
countDirectives v l = DL.length $ filter (isEntry v) l
  where
    isEntry :: String -> (String, String) -> Bool
    isEntry name (name', _) | name == name' = True
                            | otherwise     = False


-- Returns the given directive in the list of pairs, or Nothing.
lookupDirective :: String -> [(String, String)] -> Maybe String
lookupDirective d []                      = Nothing
lookupDirective d ((d', v):t) | d == d'   = Just v
                              | otherwise = lookupDirective d t


-- Returns the given directive in the list of pairs, or the default value
-- otherwise.
lookupDirectiveWithDefault :: String -> [(String, String)] -> String -> String
lookupDirectiveWithDefault di l de
  | lookup == Nothing = de
  | otherwise         = let Just r = lookup in r
  where
    lookup = lookupDirective di l


-- Implementation of "Hi()" as specified in the Notation section of RFC 5802
-- ("SCRAM"). It takes a string "str", a salt, and an interation count, and
-- returns an octet string. The iteration count must be greater than zero.

hi :: ByteString -> ByteString -> Integer -> ByteString

hi str salt i | i > 0 = xorUs $ us (concat [salt, runPutLazy $ putWord32be 1]) i
    where

        -- Calculates the U's (U1 ... Ui) using the HMAC algorithm
        us :: ByteString -> Integer -> [ByteString]
        us a 1 = [encodeLazy $ (hmac (MacKey (head $ toChunks str)) a :: SHA1)]
        us a x = [encodeLazy $ (hmac (MacKey (head $ toChunks str)) a :: SHA1)] ++ (us (encodeLazy $ (hmac (MacKey (head $ toChunks str)) a :: SHA1)) (x - 1))

        -- XORs the ByteStrings: U1 XOR U2 XOR ... XOR Ui
        xorUs :: [ByteString] -> ByteString
        xorUs (b:bs) = foldl (\ x y -> pack $ zipWith xor x y) b bs


-- TODO: Implement SCRAM.

replyToChallenge = replyToChallenge


-- Stripts the quotations around a string, if any; "\"hello\"" becomes "hello".

stripQuotations :: String -> String
stripQuotations ""                                     = ""
stripQuotations s | (head s == '"') && (last s == '"') = tail $ init s
                  | otherwise                          = s


saslprepProfile :: StringPrepProfile

saslprepProfile = Profile { maps = [\ char -> if char `inRanges` (ranges c12) then singleton '\x0020' else empty, b1]
                          , shouldNormalize = True
                          , prohibited = [a1] ++ [c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
                          , shouldCheckBidi = True }
