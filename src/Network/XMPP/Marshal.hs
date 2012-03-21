{-# Language OverloadedStrings, ViewPatterns, NoMonomorphismRestriction #-}

module Network.XMPP.Marshal where

import Control.Applicative((<$>))

import Data.Maybe
import Data.Text(Text)

import qualified Data.Text as Text

import Network.XMPP.Pickle
import Network.XMPP.Types

import Text.XML.Expat.Pickle

stanzaSel (SMessage  _ )= 0
stanzaSel (SPresence _ )= 1
stanzaSel (SIQ       _ )= 2

stanzaP :: PU [Node Text Text] Stanza
stanzaP = xpAlt stanzaSel
               [ xpWrap (SMessage  , (\(SMessage  m) -> m)) messageP
               , xpWrap (SPresence , (\(SPresence p) -> p)) presenceP
               , xpWrap (SIQ       , (\(SIQ       i) -> i)) iqP
               ]

messageP :: PU [Node Text Text] Message
messageP = xpWrap  ( (\((from, to, id, tp),(sub, body, thr,ext))
                             -> Message from to id tp sub body thr ext)
                        , (\(Message from to id tp sub body thr ext)
                             -> ((from, to, id, tp), (sub, body, thr,ext)))
                        ) $
           xpElem "message"
             (xp4Tuple
               (xpAttrImplied "from" xpPrim)
               (xpAttr        "to"   xpPrim)
               (xpAttrImplied "id"   xpText)
               (xpAttrImplied "type" xpPrim)
             )
             (xp4Tuple
               (xpOption . xpElemNodes "subject" $ xpContent xpText)
               (xpOption . xpElemNodes "body" $ xpContent xpText)
               (xpOption . xpElemNodes "thread" $ xpContent xpText)
               xpTrees
             )

presenceP :: PU [Node Text Text] Presence
presenceP = xpWrap  ( (\((from, to, id, tp),(shw, stat, prio, ext))
                             -> Presence from to id tp shw stat prio ext)
                        , (\(Presence from to id tp shw stat prio ext)
                             -> ((from, to, id, tp), (shw, stat, prio, ext)))
                        ) $
           xpElem "presence"
             (xp4Tuple
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               (xpAttrImplied "id"   xpText)
               (xpAttrImplied "type" xpPrim)
             )
             (xp4Tuple
               (xpOption . xpElemNodes "show" $ xpContent xpPrim)
               (xpOption . xpElemNodes "status" $ xpContent xpText)
               (xpOption . xpElemNodes "priority" $ xpContent xpPrim)
               xpTrees
             )

iqP :: PU [Node Text Text] IQ
iqP = xpWrap  ( (\((from, to, id, tp),body) -> IQ from to id tp body)
                    , (\(IQ from to id tp body) -> ((from, to, id, tp), body))
                    ) $
           xpElem "iq"
             (xp4Tuple
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               (xpAttr        "id"   xpText)
               (xpAttr        "type" xpPrim))
             (xpTree)

