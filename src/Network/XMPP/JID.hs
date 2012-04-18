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

import           Control.Applicative ((<$>),(<|>))
import           Control.Monad(guard)

import qualified Data.Attoparsec.Text as AP
import           Data.Text (Text)
import qualified Data.Text as Text

-- import           Network.URI (isIPv4address, isIPv6address)
import           Network.XMPP.Types

import qualified Text.NamePrep as SP
import qualified Text.StringPrep as SP

-- |
-- Converts a string to a JID.
fromString :: String -> Maybe JID
fromString s = fromStrings l d r
    where
        Right (l, d, r) =
            AP.parseOnly jidParts (Text.pack s)

-- |
-- Converts localpart, domainpart, and resourcepart strings to a JID.
-- Runs the appropriate stringprep profiles and validates the parts.
fromStrings :: Maybe Text -> Text -> Maybe Text -> Maybe JID
fromStrings l d r = do
        localPart <- case l of
          Nothing -> return Nothing
          Just l'-> do
            l'' <- SP.runStringPrep nodeprepProfile l'
            guard $ validPartLength l''
            return $ Just l''
        domainPart <- SP.runStringPrep (SP.namePrepProfile False) d
        guard $ validDomainPart domainPart
        resourcePart <- case r of
          Nothing -> return Nothing
          Just r' -> do
            r'' <- SP.runStringPrep resourceprepProfile r'
            guard $ validPartLength r''
            return $ Just r''
        return $ JID localPart domainPart resourcePart
  where
    -- Returns the domainpart if it was a valid IP or if the toASCII
    -- function was successful, or Nothing otherwise.
    validDomainPart _s = True -- TODO
      -- isIPv4address s || isIPv6address s || validHostname s

    validPartLength :: Text -> Bool
    validPartLength p = Text.length p > 0 && Text.length p < 1024
    -- Validates a host name
    -- validHostname :: Text -> Bool
    -- validHostname _ = True -- TODO

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
jidParts :: AP.Parser (Maybe Text, Text, Maybe Text)
jidParts = do
  a <- firstPartP
  b <- Just <$> domainPartP <|> (return Nothing)
  c <- Just <$> resourcePartP <|> (return Nothing)
  case (a,b,c) of
    -- Whether or not we have a resource part, if there is no "@"
    -- x is the domain
    (x, Nothing, z) -> return (Nothing, x, z)
    -- When we do have an "@", x is the localpart
    (x, Just y, z) -> return (Just x, y, z)
  where
       firstPartP = AP.takeWhile1 (AP.notInClass ['@', '/'])
       domainPartP = do
           _ <- AP.char '@'
           AP.takeWhile1 (/= '/')
       resourcePartP = do
           _ <- AP.char '/'
           AP.takeText


nodeprepProfile :: SP.StringPrepProfile
nodeprepProfile = SP.Profile
                             { SP.maps = [SP.b1, SP.b2]
                             , SP.shouldNormalize = True
                             , SP.prohibited = [SP.a1
                                            , SP.c3
                                            , SP.c4
                                            , SP.c5
                                            , SP.c6
                                            , SP.c7
                                            , SP.c8
                                            , SP.c9
                                            , SP.c11
                                            , SP.c12
                                            , SP.c21
                                            , SP.c22
                                            ]
                             , SP.shouldCheckBidi = True
                             }

-- These needs to be checked for after normalization. We could also
-- look up the Unicode mappings and include a list of characters in
-- the prohibited field above. Let's defer that until we know that we
-- are going to use stringprep.
nodeprepExtraProhibitedCharacters :: [Char]
nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F',
                                     '\x3A', '\x3C', '\x3E', '\x40']

resourceprepProfile :: SP.StringPrepProfile
resourceprepProfile = SP.Profile
                                 { SP.maps = [SP.b1]
                                 , SP.shouldNormalize = True
                                 , SP.prohibited = [ SP.a1
                                                , SP.c3
                                                , SP.c4
                                                , SP.c5
                                                , SP.c6
                                                , SP.c7
                                                , SP.c8
                                                , SP.c9
                                                , SP.c12
                                                , SP.c21
                                                , SP.c22
                                                ]
                                 , SP.shouldCheckBidi = True
                                 }
