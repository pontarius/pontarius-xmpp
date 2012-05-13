{-# OPTIONS_HADDOCK hide #-}

-- This module deals with JIDs, also known as XMPP addresses. For more
-- information on JIDs, see RFC 6122: XMPP: Address Format.

module Network.XMPP.JID
    ( JID(..)
    , fromText
    , fromStrings
    , isBare
    , isFull
    ) where

import           Control.Applicative ((<$>),(<|>))
import           Control.Monad(guard)

import qualified Data.Attoparsec.Text as AP
import           Data.Maybe(fromJust)
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.NamePrep as SP
import qualified Text.StringPrep as SP

data JID = JID { -- | The @localpart@ of a JID is an optional identifier placed
                 -- before the domainpart and separated from the latter by a
                 -- \'\@\' character. Typically a localpart uniquely identifies
                 -- the entity requesting and using network access provided by a
                 -- server (i.e., a local account), although it can also
                 -- represent other kinds of entities (e.g., a chat room
                 -- associated with a multi-user chat service). The entity
                 -- represented by an XMPP localpart is addressed within the
                 -- context of a specific domain (i.e.,
                 -- @localpart\@domainpart@).
                 localpart :: !(Maybe Text)
                 
                 -- | The domainpart typically identifies the /home/ server to
                 -- which clients connect for XML routing and data management
                 -- functionality. However, it is not necessary for an XMPP
                 -- domainpart to identify an entity that provides core XMPP
                 -- server functionality (e.g., a domainpart can identify an
                 -- entity such as a multi-user chat service, a
                 -- publish-subscribe service, or a user directory).
               , domainpart :: !Text
                 
                 -- | The resourcepart of a JID is an optional identifier placed
                 -- after the domainpart and separated from the latter by the
                 -- \'\/\' character. A resourcepart can modify either a
                 -- @localpart\@domainpart@ address or a mere @domainpart@
                 -- address. Typically a resourcepart uniquely identifies a
                 -- specific connection (e.g., a device or location) or object
                 -- (e.g., an occupant in a multi-user chat room) belonging to
                 -- the entity associated with an XMPP localpart at a domain
                 -- (i.e., @localpart\@domainpart/resourcepart@).
               , resourcepart :: !(Maybe Text)
               } deriving Eq

instance Show JID where
  show (JID nd dmn res) =
      maybe "" ((++ "@") . Text.unpack) nd ++ Text.unpack dmn ++
          maybe "" (('/' :) . Text.unpack) res

instance Read JID where
  readsPrec _ x = case fromText (Text.pack x) of
      Nothing -> []
      Just j -> [(j,"")]

instance IsString JID where
  fromString = fromJust . fromText . Text.pack

-- | Converts a Text to a JID.
fromText :: Text -> Maybe JID
fromText t = do
    (l, d, r) <- eitherToMaybe $ AP.parseOnly jidParts t
    fromStrings l d r
  where
    eitherToMaybe = either (const Nothing) Just

-- | Converts localpart, domainpart, and resourcepart strings to a JID. Runs the
-- appropriate stringprep profiles and validates the parts.
fromStrings :: Maybe Text -> Text -> Maybe Text -> Maybe JID
fromStrings l d r = do
    localPart <- case l of
        Nothing -> return Nothing
        Just l'-> do
            l'' <- SP.runStringPrep nodeprepProfile l'
            guard $ validPartLength l''
            let prohibMap = Set.fromList nodeprepExtraProhibitedCharacters
            guard $ Text.all (`Set.notMember` prohibMap) l''
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
    validDomainPart :: Text -> Bool
    validDomainPart _s = True -- TODO

    validPartLength :: Text -> Bool
    validPartLength p = Text.length p > 0 && Text.length p < 1024

-- | Returns True if the JID is /bare/, and False otherwise.
isBare :: JID -> Bool
isBare j | resourcepart j == Nothing = True
         | otherwise                 = False

-- | Returns True if the JID is 'full', and False otherwise.
isFull :: JID -> Bool
isFull = not . isBare

-- Parses an JID string and returns its three parts. It performs no validation
-- or transformations.
jidParts :: AP.Parser (Maybe Text, Text, Maybe Text) 
jidParts = do
    -- Read until we reach an '@', a '/', or EOF.
    a <- AP.takeWhile1 (AP.notInClass ['@', '/'])
    -- Case 1: We found an '@', and thus the localpart. At least the domainpart
    -- is remaining. Read the '@' and until a '/' or EOF.
    do
        b <- domainPartP
        -- Case 1A: We found a '/' and thus have all the JID parts. Read the '/'
        -- and until EOF.
        do
            c <- resourcePartP -- Parse resourcepart
            return (Just a, b, Just c)
        -- Case 1B: We have reached EOF; the JID is in the form
        -- localpart@domainpart.
            <|> do
                AP.endOfInput
                return (Just a, b, Nothing)
          -- Case 2: We found a '/'; the JID is in the form
          -- domainpart/resourcepart.
          <|> do
              b <- resourcePartP
              AP.endOfInput
              return (Nothing, a, Just b)
          -- Case 3: We have reached EOF; we have an JID consisting of only a
          -- domainpart.
        <|> do
            AP.endOfInput
            return (Nothing, a, Nothing)
  where
    -- Read an '@' and everything until a '/'.
    domainPartP :: AP.Parser Text
    domainPartP = do
        _ <- AP.char '@'
        AP.takeWhile1 (/= '/')
    -- Read everything until a '/'.
    resourcePartP :: AP.Parser Text
    resourcePartP = do
        _ <- AP.char '/'
        AP.takeText

-- The `nodeprep' StringPrep profile.
nodeprepProfile :: SP.StringPrepProfile
nodeprepProfile = SP.Profile { SP.maps = [SP.b1, SP.b2]
                             , SP.shouldNormalize = True
                             , SP.prohibited = [SP.a1
                                               , SP.c11
                                               , SP.c12
                                               , SP.c21
                                               , SP.c22
                                               , SP.c3
                                               , SP.c4
                                               , SP.c5
                                               , SP.c6
                                               , SP.c7
                                               , SP.c8
                                               , SP.c9
                                               ]
                             , SP.shouldCheckBidi = True
                             }

-- These characters needs to be checked for after normalization.
nodeprepExtraProhibitedCharacters :: [Char]
nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F', '\x3A',
                                     '\x3C', '\x3E', '\x40']

-- The `resourceprep' StringPrep profile.
resourceprepProfile :: SP.StringPrepProfile
resourceprepProfile = SP.Profile { SP.maps = [SP.b1]
                                 , SP.shouldNormalize = True
                                 , SP.prohibited = [ SP.a1
                                                   , SP.c12
                                                   , SP.c21
                                                   , SP.c22
                                                   , SP.c3
                                                   , SP.c4
                                                   , SP.c5
                                                   , SP.c6
                                                   , SP.c7
                                                   , SP.c8
                                                   , SP.c9
                                                   ]
                                 , SP.shouldCheckBidi = True
                                 }