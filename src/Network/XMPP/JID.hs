-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
-- Pontarius distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

-- TODO: When no longer using stringprep, do appropriate testing.
-- (Including testing addresses like a@b/c@d/e, a/b@c, a@/b, a/@b...)
-- Will we not be using stringprep?

-- TODO: Unicode 3.2 should be used.


-- This module deals with XMPP addresses, also known as JIDs. For more
-- information on JIDs, see RFC 6122: XMPP: Address Format.
--
-- This module does not internationalize hostnames.


module Network.XMPP.JID (fromString, fromStrings, isBare, isFull) where

import Network.XMPP.Types

import Data.Maybe (fromJust, isJust)
import Text.Parsec ((<|>), anyToken, char, eof, many, noneOf, parse)
import Text.Parsec.ByteString (GenParser)

import Text.StringPrep (StringPrepProfile (..), a1, b1, b2, c11, c12, c21, c22,
                        c3, c4, c5, c6, c7, c8, c9, runStringPrep)
import Text.NamePrep (namePrepProfile)

import Network.URI (isIPv4address, isIPv6address)

import qualified Data.ByteString.Char8 as DBC (pack)
import qualified Data.Text as DT (pack, unpack)


-- |
-- Converts a string to a JID.

fromString :: String -> Maybe JID

fromString s = fromStrings localpart domainpart resourcepart
    where
        Right (localpart, domainpart, resourcepart) =
            parse jidParts "" (DBC.pack s)


-- |
-- Converts localpart, domainpart, and resourcepart strings to a JID.

-- Runs the appropriate stringprep profiles and validates the parts.

fromStrings :: Maybe String -> String -> Maybe String -> Maybe JID

fromStrings l s r
  | domainpart == Nothing = Nothing
  | otherwise = if validateNonDomainpart localpart &&
                   isJust domainpart' &&
                   validateNonDomainpart resourcepart
                then Just (JID localpart (fromJust domainpart') resourcepart)
                else Nothing
  where
    
    -- Applies the nodeprep profile on the localpart string, if any.
    localpart :: Maybe String
    localpart = case l of
      Just l' -> case runStringPrep nodeprepProfile (DT.pack l') of
        Just l'' -> Just $ DT.unpack l''
        Nothing -> Nothing
      Nothing -> Nothing

    -- Applies the nameprep profile on the domainpart string.
    -- TODO: Allow unassigned?
    domainpart :: Maybe String
    domainpart = case runStringPrep (namePrepProfile False) (DT.pack s) of
      Just s' -> Just $ DT.unpack s'
      Nothing -> Nothing

    -- Applies the resourceprep profile on the resourcepart string, if
    -- any.
    resourcepart :: Maybe String
    resourcepart = case r of
      Just r' -> case runStringPrep resourceprepProfile (DT.pack r') of
        Just r'' -> Just $ DT.unpack r''
        Nothing -> Nothing
      Nothing -> Nothing

    -- Returns the domainpart if it was a valid IP or if the toASCII
    -- function was successful, or Nothing otherwise.
    domainpart' :: Maybe String
    domainpart' | isIPv4address s || isIPv6address s = Just s
                | validHostname s = Just s
                | otherwise = Nothing

    -- Validates that non-domainpart strings have an appropriate
    -- length.
    validateNonDomainpart :: Maybe String -> Bool
    validateNonDomainpart Nothing = True
    validateNonDomainpart (Just l) = validPartLength l
      where
        validPartLength :: String -> Bool
        validPartLength p = length p > 0 && length p < 1024

    -- Validates a host name
    validHostname :: String -> Bool
    validHostname _ = True -- TODO


-- | Returns True if the JID is `bare', and False otherwise.

isBare :: JID -> Bool

isBare j | resourcepart j == Nothing = True
         | otherwise                 = False


-- | Returns True if the JID is `full', and False otherwise.

isFull :: JID -> Bool

isFull jid = not $ isBare jid


-- Parses an JID string and returns its three parts. It performs no
-- validation or transformations. We are using Parsec to parse the
-- JIDs. There is no input for which 'jidParts' fails.

jidParts :: GenParser Char st (Maybe String, String, Maybe String)

jidParts = do

  -- Read until we reach an '@', a '/', or EOF.
  a <- many $ noneOf ['@', '/']

  -- Case 1: We found an '@', and thus the localpart. At least the
  -- domainpart is remaining. Read the '@' and until a '/' or EOF.
  do
    char '@'
    b <- many $ noneOf ['/']

    -- Case 1A: We found a '/' and thus have all the JID parts. Read
    -- the '/' and until EOF.
    do
      char '/' -- Resourcepart remaining
      c <- many $ anyToken -- Parse resourcepart
      eof
      return (Just a, b, Just c)

    -- Case 1B: We have reached EOF; the JID is in the form
    -- localpart@domainpart.
      <|> do
        eof
        return (Just a, b, Nothing)

    -- Case 2: We found a '/'; the JID is in the form
    -- domainpart/resourcepart.
    <|> do
      char '/'
      b <- many $ anyToken
      eof
      return (Nothing, a, Just b)

    -- Case 3: We have reached EOF; we have an JID consisting of only
    -- a domainpart.
    <|> do
      eof
      return (Nothing, a, Nothing)


nodeprepProfile :: StringPrepProfile

nodeprepProfile = Profile { maps = [b1, b2]
                          , shouldNormalize = True
                          , prohibited = [a1] ++ [c11, c12, c21, c22,
                                                  c3, c4, c5, c6, c7,
                                                  c8, c9]
                          , shouldCheckBidi = True }


-- These needs to be checked for after normalization. We could also
-- look up the Unicode mappings and include a list of characters in
-- the prohibited field above. Let's defer that until we know that we
-- are going to use stringprep.

nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F',
                                     '\x3A', '\x3C', '\x3E', '\x40']



resourceprepProfile :: StringPrepProfile

resourceprepProfile = Profile { maps = [b1]
                              , shouldNormalize = True
                              , prohibited = [a1] ++ [c12, c21, c22,
                                                      c3, c4, c5, c6,
                                                      c7, c8, c9]
                              , shouldCheckBidi = True }
