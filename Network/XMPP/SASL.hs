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

-- TODO: Make it possible to include host.
-- TODO: Host is assumed to be ISO 8859-1; make list of assumptions.
-- TODO: Can it contain newline characters?

module Network.XMPP.SASL (replyToChallenge1) where

import Data.ByteString.Internal (c2w)
import Data.Char (isLatin1)
import Data.Digest.Pure.MD5
import qualified Data.Binary as DBi (Binary, encode)
import qualified Data.ByteString.Lazy as DBL (ByteString, append, pack,
                                              fromChunks, toChunks, null)
import qualified Data.ByteString.Lazy.Char8 as DBLC (append, pack, unpack)
import qualified Data.List as DL


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


-- Takes a challenge string (which is not Base64-encoded), the host name of the
-- Jabber server, the Jabber user name (JID), the password and a random and
-- unique "cnonce" value and generates either an error or a response to that
-- challenge.

-- We have broken replyToChallenge1 for non-TLS authentication. In order to
-- change it back, just uncomment the lines relevant to the realm and match it
-- in the C1NotAllParametersSet case.

replyToChallenge1 :: String -> String -> String -> String -> String ->
                     Either String Challenge1Error
replyToChallenge1 s h u p c =
  -- Remove all new line characters.
  let list = stringToList $ filter (/= '\n') s
  in -- Count that there are no more than one nonce or algorithm directives.
     case countDirectives "nonce"     list <= 1 &&
          countDirectives "algorithm" list <= 1 of
       True ->
         let -- realm     = lookupDirective "realm" list
             nonce     = lookupDirective "nonce" list
             qop       = lookupDirectiveWithDefault "qop" list "auth"
             charset   = lookupDirectiveWithDefault "charset" list "utf-8"
             algorithm = lookupDirective "algorithm" list
         
         -- Verify that all necessary directives has been set.
         in case (nonce, qop, charset, algorithm) of
              (Just nonce', qop', charset', Just algorithm') ->
                
                -- Strip quotations of the directives that need it.
                let -- realm'' = stripQuotations realm'
                    nonce'' = stripQuotations nonce'
                    qop'' = stripQuotations qop' -- It seems ejabberd gives us an errorous "auth" instead of auth
                in
                -- -- Verify that the realm is the same as the Jabber host.
                -- case realm'' == h of
                --      True ->
                       
                       -- Verify that QOP is "auth", charset is "utf-8" and that
                       -- the algorithm is "md5-sess".
                       case qop'' == "auth" of
                         True ->
                           case charset' == "utf-8" of
                             True ->
                               case algorithm' == "md5-sess" of
                                 True ->
                                 
                                   -- All data is valid; generate the reply.
                                   Left (reply nonce'' qop'')
                                 
                                 -- Errors are caught and reported below.
                                 False -> Right C1UnsupportedAlgorithm
                             False -> Right C1UnsupportedCharset
                         False -> Right C1UnsupportedQOP
                     -- False -> Right C1WrongRealm
              _ -> Right C1NotAllParametersPresent
  where
    reply n q =
      let -- We start with what's in RFC 2831 is referred to as "A1", a 16 octet
          -- MD5 hash.
          
          -- If the username or password values are in ISO-8859-1, we convert
          -- them to ISO-8859-1 strings.
          username = case all isLatin1 u of
            True -> DBL.pack $ map c2w u
            False -> DBLC.pack $ u
          password = case all isLatin1 p of
            True -> DBL.pack $ map c2w p
            False -> DBLC.pack p
          
          nc = "00000001"
          digestUri = "xmpp/" ++ h
          
          -- Build the "{ username-value, ":", realm-value, ":", passwd }"
          -- bytestring, the rest of the bytestring and then join them.
          a1a = DBi.encode $ md5 $ DBLC.append
                (DBLC.append username (DBLC.pack (":" ++ h ++ ":")))
                password
          a1aDebug = "DBi.encode $ md5 $ " ++ (DBLC.unpack $ DBLC.append
                (DBLC.append username (DBLC.pack (":" ++ h ++ ":")))
                password)
          a1b = DBLC.pack (":" ++ n ++ ":" ++ c)
          a1 = DBLC.append a1a a1b
          
          -- Generate the "A2" value.
          a2 = DBLC.pack ("AUTHENTICATE:" ++ digestUri)
          
          -- Produce the responseValue.
          k = DBLC.pack (show $ md5 a1)
          colon = DBLC.pack ":"
          s0 = DBLC.pack (n ++ ":" ++ nc ++ ":" ++ c ++ ":" ++
                          q ++ ":")
          s1 = DBLC.pack $ show $ md5 a2
          
          s_ = DBLC.append s0 s1
          -- append k:d and 16 octet hash it
          kd = md5 (DBLC.append k (DBLC.append colon s_))
          
          lol0 = DBLC.unpack s_
          lol1 = show kd
          
          response = show kd
      in "username=\"" ++ u ++ "\",realm=\"" ++ h ++ "\",nonce=\"" ++ n ++
         "\",cnonce=\"" ++ c ++ "\",nc=" ++ nc ++ ",digest-uri=\"" ++
         digestUri ++ "\",qop=auth,response=" ++ response ++ ",charset=utf-8"
         -- "\n\n" ++
         -- "a1aDebug: " ++ a1aDebug ++ "\n" ++
         -- "a1b: " ++ (DBLC.unpack a1b) ++ "\n" ++
         -- "a1: " ++ (DBLC.unpack a1) ++ "\n" ++
         -- "a2: " ++ (DBLC.unpack a2) ++ "\n" ++
         -- "k: " ++ (DBLC.unpack k) ++ "\n" ++
         -- "colon: " ++ (DBLC.unpack colon) ++ "\n" ++
         -- "s0: " ++ (DBLC.unpack s0) ++ "\n" ++
         -- "s1: " ++ (DBLC.unpack s1) ++ "\n" ++
         -- "s_: " ++ (DBLC.unpack s_) ++ "\n"


-- Stripts the quotations around a string, if any; "\"hello\"" becomes "hello".

stripQuotations :: String -> String
stripQuotations ""                                     = ""
stripQuotations s | (head s == '"') && (last s == '"') = tail $ init s
                  | otherwise                          = s
