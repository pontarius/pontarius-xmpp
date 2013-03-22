module Network.Xmpp.IM
  ( -- * Instant Messages
    subject
  , thread
  , body
  , bodies
  , newIM
  , simpleIM
  , answerIM
    -- * Presence
  , module Network.Xmpp.IM.Presence
  -- * Roster
  , Roster(..)
  , Item(..)
  , getRoster
  ) where

import Network.Xmpp.IM.Message
import Network.Xmpp.IM.Presence
import Network.Xmpp.IM.Roster
import Network.Xmpp.IM.Roster.Types
