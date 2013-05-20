{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.Xmpp.IM.Presence where

import           Data.Text (Text)
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp.Types

data ShowStatus = StatusAway
                | StatusChat
                | StatusDnd
                | StatusXa

instance Show ShowStatus where
    show StatusAway = "away"
    show StatusChat = "chat"
    show StatusDnd  = "dnd"
    show StatusXa   = "xa"

instance Read ShowStatus where
    readsPrec _ "away" = [(StatusAway, "")]
    readsPrec _ "chat" = [(StatusChat, "")]
    readsPrec _ "dnd"  = [(StatusDnd , "")]
    readsPrec _ "xa"   = [(StatusXa  , "")]
    readsPrec _ _      = []

data IMPresence = IMP { showStatus :: Maybe ShowStatus
                      , status     :: Maybe Text
                      , priority   :: Maybe Int
                      } deriving Show

imPresence :: IMPresence
imPresence = IMP { showStatus = Nothing
                 , status     = Nothing
                 , priority   = Nothing
                 }


getIMPresence :: Presence -> Maybe IMPresence
getIMPresence pres = case unpickle xpIMPresence (presencePayload pres) of
    Left _ -> Nothing
    Right r -> Just r

withIMPresence :: IMPresence -> Presence -> Presence
withIMPresence imPres pres = pres{presencePayload = presencePayload pres
                                                   ++ pickleTree xpIMPresence
                                                                 imPres}

--
-- Picklers
--

xpIMPresence :: PU [Element] IMPresence
xpIMPresence = xpUnliftElems .
               xpWrap (\(s, st, p) -> IMP s st p)
                      (\(IMP s st p) -> (s, st, p)) .
               xpClean $
               xp3Tuple
                  (xpOption $ xpElemNodes "{jabber:client}show"
                     (xpContent xpPrim))
                  (xpOption $ xpElemNodes "{jabber:client}status"
                     (xpContent xpText))
                  (xpOption $ xpElemNodes "{jabber:client}priority"
                     (xpContent xpPrim))
