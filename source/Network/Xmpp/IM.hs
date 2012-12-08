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
  ) where

import Network.Xmpp.IM.Message
import Network.Xmpp.IM.Presence
