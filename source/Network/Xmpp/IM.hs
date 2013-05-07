-- | RFC 6121: Instant Messaging and Presence
--
module Network.Xmpp.IM
  ( -- * Instant Messages
    InstantMessage(..)
  , MessageBody(..)
  , MessageThread(..)
  , MessageSubject(..)
  , instantMessage
  , simpleIM
  , getIM
  , withIM
  , answerIM
     -- * Presence
  , ShowStatus(..)
  , IMPresence(..)
  , imPresence
  , getIMPresence
  , withIMPresence
  -- * Roster
  , Roster(..)
  , Item(..)
  , getRoster
  , rosterAdd
  , rosterRemove
  ) where

import Network.Xmpp.IM.Message
import Network.Xmpp.IM.Presence
import Network.Xmpp.IM.Roster
import Network.Xmpp.IM.Roster.Types
