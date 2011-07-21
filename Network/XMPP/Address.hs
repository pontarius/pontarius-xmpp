-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

-- TODO: Move away from stringprep for all three profiles.

-- TODO: When no longer using stringprep, do appropriate testing. (Including
-- testing addresses like a@b/c@d/e, a/b@c, a@/b, a/@b...)

-- TODO: Unicode 3.2 should be used.


-- This module deals with XMPP addresses (also known as JIDs and JabberIDs). For
-- more information on XMPP addresses, see RFC 6122: XMPP: Address Format.
--
-- Provided hostnames may contain international characters; Pontarius XMPP will
-- try to convert such hostnames to internationalized hostnames.


module Network.XMPP.Address (fromString, fromStrings, isBare, isFull) where

import Network.XMPP.Types

import Data.Maybe (fromJust, isJust)
import Text.Parsec ((<|>), anyToken, char, eof, many, noneOf, parse)
import Text.Parsec.ByteString (GenParser)

import Text.StringPrep (StringPrepProfile (..), a1, b1, b2, c11, c12, c21, c22,
                        c3, c4, c5, c6, c7, c8, c9, runStringPrep)
import Text.NamePrep (namePrepProfile)

import Data.Text.IDNA2008 (toASCII)

import Network.URI (isIPv4address, isIPv6address)

import qualified Data.ByteString.Char8 as DBC (pack)
import qualified Data.Text as DT (pack, unpack)


-- |
-- Converts a string to an XMPP address.

fromString :: String -> Maybe Address

fromString s = fromStrings localpart domainpart resourcepart
    where
        Right (localpart, domainpart, resourcepart) =
            parse addressParts "" (DBC.pack s)


-- |
-- Converts localpart, domainpart, and resourcepart strings to an XMPP address.

-- Runs the appropriate stringprep profiles and validates the parts.

fromStrings :: Maybe String -> String -> Maybe String -> Maybe Address

fromStrings l s r
    | domainpart == Nothing = Nothing
    | otherwise = if validateNonDomainpart localpart &&
                     isJust domainpart' &&
                     validateNonDomainpart resourcepart
                  then Just (Address localpart (fromJust domainpart') resourcepart)
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

        -- Applies the resourceprep profile on the resourcepart string, if any.
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
                    | otherwise = toASCII s

        -- Validates that non-domainpart strings have an appropriate length.
        validateNonDomainpart :: Maybe String -> Bool
        validateNonDomainpart Nothing = True
        validateNonDomainpart (Just l) = validPartLength l
            where
                validPartLength :: String -> Bool
                validPartLength p = length p > 0 && length p < 1024


-- | Returns True if the address is `bare', and False otherwise.

isBare :: Address -> Bool

isBare j | resourcepart j == Nothing = True
         | otherwise                 = False


-- | Returns True if the address is `full', and False otherwise.

isFull :: Address -> Bool

isFull jid = not $ isBare jid


-- Parses an address string and returns its three parts. It performs no
-- validation or transformations. We are using Parsec to parse the address.
-- There is no input for which 'addressParts' fails.

addressParts :: GenParser Char st (Maybe String, String, Maybe String)

addressParts = do

    -- Read until we reach an '@', a '/', or EOF.
    a <- many $ noneOf ['@', '/']

    -- Case 1: We found an '@', and thus the localpart. At least the domainpart
    -- is remaining. Read the '@' and until a '/' or EOF.
    do
        char '@'
        b <- many $ noneOf ['/']

        -- Case 1A: We found a '/' and thus have all the address parts. Read the
        -- '/' and until EOF.
        do
            char '/' -- Resourcepart remaining
            c <- many $ anyToken -- Parse resourcepart
            eof
            return (Just a, b, Just c)

        -- Case 1B: We have reached EOF; the address is in the form
        -- localpart@domainpart.
            <|> do
                eof
                return (Just a, b, Nothing)

        -- Case 2: We found a '/'; the address is in the form
        -- domainpart/resourcepart.
        <|> do
            char '/'
            b <- many $ anyToken
            eof
            return (Nothing, a, Just b)

        -- Case 3: We have reached EOF; we have an address consisting of only a
        -- domainpart.
        <|> do
            eof
            return (Nothing, a, Nothing)


nodeprepProfile :: StringPrepProfile

nodeprepProfile = Profile { maps = [b1, b2]
                          , shouldNormalize = True
                          , prohibited = [a1] ++ [c11, c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
                          , shouldCheckBidi = True }


-- These needs to be checked for after normalization. We could also look up the
-- Unicode mappings and include a list of characters in the prohibited field
-- above. Let's defer that until we know that we are going to use stringprep.

nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F', '\x3A',
                                     '\x3C', '\x3E', '\x40']



resourceprepProfile :: StringPrepProfile

resourceprepProfile = Profile { maps = [b1]
                          , shouldNormalize = True
                          , prohibited = [a1] ++ [c12, c21, c22, c3, c4, c5, c6, c7, c8, c9]
                          , shouldCheckBidi = True }
