{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Presence where

import Data.Text(Text)
import Network.Xmpp.Types

-- | Add a recipient to a presence notification.
presTo :: Presence -> JID -> Presence
presTo pres to = pres{presenceTo = Just to}