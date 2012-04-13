{-# Language OverloadedStrings, ViewPatterns, NoMonomorphismRestriction #-}

module Network.XMPP.Marshal where

import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types

stanzaSel :: Stanza -> Int
stanzaSel (IQRequestS     _) = 0
stanzaSel (IQResultS      _) = 1
stanzaSel (IQErrorS       _) = 2
stanzaSel (MessageS       _) = 3
stanzaSel (MessageErrorS  _) = 4
stanzaSel (PresenceS      _) = 5
stanzaSel (PresenceErrorS _) = 6

stanzaP :: PU [Node] Stanza
stanzaP = xpAlt stanzaSel
    [ xpWrap IQRequestS     (\(IQRequestS     x) -> x) xpIQRequest
    , xpWrap IQResultS      (\(IQResultS      x) -> x) xpIQResult
    , xpWrap IQErrorS       (\(IQErrorS       x) -> x) xpIQError
    , xpWrap MessageS       (\(MessageS       x) -> x) xpMessage
    , xpWrap MessageErrorS  (\(MessageErrorS  x) -> x) xpMessageError
    , xpWrap PresenceS      (\(PresenceS      x) -> x) xpPresence
    , xpWrap PresenceErrorS (\(PresenceErrorS x) -> x) xpPresenceError
    ]

xmlLang :: Name
xmlLang = Name "lang" Nothing (Just "xml")

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpPrim

xpMessage :: PU [Node] (Message)
xpMessage = xpWrap   (\((tp, qid, from, to, lang), (sub, body, thr, ext))
                             -> Message qid from to lang tp sub thr body ext)
                    (\(Message qid from to lang tp sub thr body ext)
                             -> ((tp, qid, from, to, lang), (sub, body, thr, ext)))
                    $
           xpElem "{jabber:client}message"
             (xp5Tuple
               (xpDefault Normal $ xpAttr "type" xpPrim)
               (xpAttrImplied "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               (xpAttrImplied xmlLang xpPrim)
                 -- TODO: NS?
             )
             (xp4Tuple
               (xpOption . xpElemNodes "{jabber:client}subject" $ xpContent xpId)
               (xpOption . xpElemNodes "{jabber:client}body" $ xpContent xpId)
               (xpOption . xpElemNodes "{jabber:client}thread" $ xpContent xpId)
               (xpAll xpElemVerbatim)
             )


xpPresence :: PU [Node] Presence
xpPresence = xpWrap   (\((qid, from, to, lang, tp),(shw, stat, prio, ext))
                             -> Presence qid from to lang tp shw stat prio ext)
                     (\(Presence qid from to lang tp shw stat prio ext)
                             -> ((qid, from, to, lang, tp), (shw, stat, prio, ext)))
                     $
           xpElem "{jabber:client}presence"
             (xp5Tuple
               (xpAttrImplied "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               xpLangTag
               (xpAttrImplied "type" xpPrim)
             )
             (xp4Tuple
               (xpOption . xpElemNodes "{jabber:client}show" $ xpContent xpPrim)
               (xpOption . xpElemNodes "{jabber:client}status" $ xpContent xpId)
               (xpOption . xpElemNodes "{jabber:client}priority" $ xpContent xpPrim)
               (xpAll xpElemVerbatim)
             )

xpIQRequest :: PU [Node] IQRequest
xpIQRequest = xpWrap  (\((qid, from, to, lang, tp),body)
                          -> IQRequest qid from to lang tp body)
              (\(IQRequest qid from to lang tp body)
                 -> ((qid, from, to, lang, tp), body))
              $
           xpElem "{jabber:client}iq"
             (xp5Tuple
               (xpAttr        "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               xpLangTag
               ((xpAttr        "type" xpPrim))
             )
             (xpElemVerbatim)

xpIQResult :: PU [Node] IQResult
xpIQResult = xpWrap  (\((qid, from, to, lang, _tp),body)
                          -> IQResult qid from to lang body)
              (\(IQResult qid from to lang body)
                 -> ((qid, from, to, lang, ()), body))
              $
           xpElem "{jabber:client}iq"
             (xp5Tuple
               (xpAttr        "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               xpLangTag
               ((xpAttrFixed "type" "result"))
             )
             (xpOption xpElemVerbatim)

----------------------------------------------------------
-- Errors
----------------------------------------------------------

xpErrorCondition :: PU [Node] StanzaErrorCondition
xpErrorCondition = xpWrap (\(cond, (), ()) -> cond) (\cond -> (cond, (), ())) $
             xpElemByNamespace
                 "urn:ietf:params:xml:ns:xmpp-stanzas" xpPrim
                  xpUnit
                  xpUnit

xpStanzaError :: PU [Node] StanzaError
xpStanzaError = xpWrap
                 (\(tp, (cond, txt, ext)) -> StanzaError tp cond txt ext)
                 (\(StanzaError tp cond txt ext) -> (tp, (cond, txt, ext))) $
                 xpElem "{jabber:client}error"
                     (xpAttr "type" xpPrim)
                     (xp3Tuple
                        xpErrorCondition
                        (xpOption $ xpElem "{jabber:client}text"
                           (xpAttrImplied xmlLang xpPrim)
                           (xpContent xpId)
                        )
                     (xpOption xpElemVerbatim)
                     )

xpMessageError :: PU [Node] (MessageError)
xpMessageError = xpWrap   (\((_, qid, from, to, lang), (err, ext))
                           -> MessageError qid from to lang err ext)
                    (\(MessageError qid from to lang err ext)
                           -> (((), qid, from, to, lang), (err, ext)))
                    $
           xpElem "{jabber:client}message"
             (xp5Tuple
               (xpAttrFixed "type"  "error")
               (xpAttrImplied "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               (xpAttrImplied xmlLang xpPrim)
                 -- TODO: NS?
             )
             (xp2Tuple
               xpStanzaError
               (xpAll xpElemVerbatim)
             )

xpPresenceError :: PU [Node] PresenceError
xpPresenceError = xpWrap   (\((qid, from, to, lang, _),(err, ext))
                        -> PresenceError qid from to lang err ext)
                     (\(PresenceError qid from to lang err ext)
                        -> ((qid, from, to, lang, ()), (err, ext)))
                     $
           xpElem "{jabber:client}presence"
             (xp5Tuple
               (xpAttrImplied "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               xpLangTag
               (xpAttrFixed "type" "error")
             )
             (xp2Tuple
               xpStanzaError
               (xpAll xpElemVerbatim)
             )

xpIQError :: PU [Node] IQError
xpIQError = xpWrap  (\((qid, from, to, lang, _tp),(err, body))
                          -> IQError qid from to lang err body)
              (\(IQError qid from to lang err body)
                 -> ((qid, from, to, lang, ()), (err, body)))
              $
           xpElem "{jabber:client}iq"
             (xp5Tuple
               (xpAttr        "id"   xpPrim)
               (xpAttrImplied "from" xpPrim)
               (xpAttrImplied "to"   xpPrim)
               xpLangTag
               ((xpAttrFixed "type" "error"))
             )
             (xp2Tuple
              xpStanzaError
              (xpOption xpElemVerbatim)
             )

