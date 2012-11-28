module Network.Xmpp.IM
  ( -- * Instant Messages
    subject
  , thread
  , body
  , newIM
  , simpleIM
  , answerIM
  -- * Presence
  , module Network.Xmpp.IM.Presence
  ) where

import Network.Xmpp.IM.Message
import Network.Xmpp.IM.Presence