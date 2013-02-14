-- Picklers and unpicklers convert Haskell data to XML and XML to Haskell data,
-- respectively. By convensions, pickler/unpickler ("PU") function names start
-- out with "xp".

{-# Language OverloadedStrings, ViewPatterns, NoMonomorphismRestriction #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Marshal where

import Data.XML.Pickle
import Data.XML.Types

import Data.Text

import Network.Xmpp.Types

xpStreamStanza :: PU [Node] (Either StreamErrorInfo Stanza)
xpStreamStanza = xpEither xpStreamError xpStanza

xpStanza :: PU [Node] Stanza
xpStanza = xpAlt stanzaSel
    [ xpWrap IQRequestS     (\(IQRequestS     x) -> x) xpIQRequest
    , xpWrap IQResultS      (\(IQResultS      x) -> x) xpIQResult
    , xpWrap IQErrorS       (\(IQErrorS       x) -> x) xpIQError
    , xpWrap MessageS       (\(MessageS       x) -> x) xpMessage
    , xpWrap MessageErrorS  (\(MessageErrorS  x) -> x) xpMessageError
    , xpWrap PresenceS      (\(PresenceS      x) -> x) xpPresence
    , xpWrap PresenceErrorS (\(PresenceErrorS x) -> x) xpPresenceError
    ]
  where
    -- Selector for which pickler to execute above.
    stanzaSel :: Stanza -> Int
    stanzaSel (IQRequestS     _) = 0
    stanzaSel (IQResultS      _) = 1
    stanzaSel (IQErrorS       _) = 2
    stanzaSel (MessageS       _) = 3
    stanzaSel (MessageErrorS  _) = 4
    stanzaSel (PresenceS      _) = 5
    stanzaSel (PresenceErrorS _) = 6

xpMessage :: PU [Node] (Message)
xpMessage = xpWrap
    (\((tp, qid, from, to, lang), ext) -> Message qid from to lang tp ext)
    (\(Message qid from to lang tp ext) -> ((tp, qid, from, to, lang), ext))
    (xpElem "{jabber:client}message"
         (xp5Tuple
             (xpDefault Normal $ xpAttr "type" xpPrim)
             (xpAttrImplied "id"   xpPrim)
             (xpAttrImplied "from" xpPrim)
             (xpAttrImplied "to"   xpPrim)
             xpLangTag
             -- TODO: NS?
         )
         (xpAll xpElemVerbatim)
    )

xpPresence :: PU [Node] Presence
xpPresence = xpWrap
    (\((qid, from, to, lang, tp), ext) -> Presence qid from to lang tp ext)
    (\(Presence qid from to lang tp ext) -> ((qid, from, to, lang, tp), ext))
    (xpElem "{jabber:client}presence"
         (xp5Tuple
              (xpAttrImplied "id"   xpPrim)
              (xpAttrImplied "from" xpPrim)
              (xpAttrImplied "to"   xpPrim)
              xpLangTag
              (xpAttrImplied "type" xpPrim)
         )
         (xpAll xpElemVerbatim)
    )

xpIQRequest :: PU [Node] IQRequest
xpIQRequest = xpWrap
    (\((qid, from, to, lang, tp),body) -> IQRequest qid from to lang tp body)
    (\(IQRequest qid from to lang tp body) -> ((qid, from, to, lang, tp), body))
    (xpElem "{jabber:client}iq"
         (xp5Tuple
             (xpAttr        "id"   xpPrim)
             (xpAttrImplied "from" xpPrim)
             (xpAttrImplied "to"   xpPrim)
             xpLangTag
             ((xpAttr        "type" xpPrim))
         )
         xpElemVerbatim
    )

xpIQResult :: PU [Node] IQResult
xpIQResult = xpWrap
    (\((qid, from, to, lang, _tp),body) -> IQResult qid from to lang body)
    (\(IQResult qid from to lang body) -> ((qid, from, to, lang, ()), body))
    (xpElem "{jabber:client}iq"
         (xp5Tuple
             (xpAttr        "id"   xpPrim)
             (xpAttrImplied "from" xpPrim)
             (xpAttrImplied "to"   xpPrim)
             xpLangTag
             ((xpAttrFixed "type" "result"))
         )
         (xpOption xpElemVerbatim)
    )

----------------------------------------------------------
-- Errors
----------------------------------------------------------

xpErrorCondition :: PU [Node] StanzaErrorCondition
xpErrorCondition = xpWrap
    (\(cond, (), ()) -> cond)
    (\cond -> (cond, (), ()))
    (xpElemByNamespace
        "urn:ietf:params:xml:ns:xmpp-stanzas"
        xpPrim
        xpUnit
        xpUnit
    )

xpStanzaError :: PU [Node] StanzaError
xpStanzaError = xpWrap
    (\(tp, (cond, txt, ext)) -> StanzaError tp cond txt ext)
    (\(StanzaError tp cond txt ext) -> (tp, (cond, txt, ext)))
    (xpElem "{jabber:client}error"
         (xpAttr "type" xpPrim)
         (xp3Tuple
              xpErrorCondition
              (xpOption $ xpElem "{jabber:client}text"
                   (xpAttrImplied xmlLang xpPrim)
                   (xpContent xpId)
              )
              (xpOption xpElemVerbatim)
         )
    )

xpMessageError :: PU [Node] (MessageError)
xpMessageError = xpWrap
    (\((_, qid, from, to, lang), (err, ext)) ->
        MessageError qid from to lang err ext)
    (\(MessageError qid from to lang err ext) ->
        (((), qid, from, to, lang), (err, ext)))
    (xpElem "{jabber:client}message"
         (xp5Tuple
              (xpAttrFixed   "type" "error")
              (xpAttrImplied "id"   xpPrim)
              (xpAttrImplied "from" xpPrim)
              (xpAttrImplied "to"   xpPrim)
              (xpAttrImplied xmlLang xpPrim)
              -- TODO: NS?
         )
         (xp2Tuple xpStanzaError (xpAll xpElemVerbatim))
    )

xpPresenceError :: PU [Node] PresenceError
xpPresenceError = xpWrap
    (\((qid, from, to, lang, _),(err, ext)) ->
        PresenceError qid from to lang err ext)
    (\(PresenceError qid from to lang err ext) ->
        ((qid, from, to, lang, ()), (err, ext)))
    (xpElem "{jabber:client}presence"
         (xp5Tuple
              (xpAttrImplied "id"   xpPrim)
              (xpAttrImplied "from" xpPrim)
              (xpAttrImplied "to"   xpPrim)
              xpLangTag
              (xpAttrFixed "type" "error")
         )
         (xp2Tuple xpStanzaError (xpAll xpElemVerbatim))
    )

xpIQError :: PU [Node] IQError
xpIQError = xpWrap
    (\((qid, from, to, lang, _tp),(err, body)) ->
        IQError qid from to lang err body)
    (\(IQError qid from to lang err body) ->
        ((qid, from, to, lang, ()), (err, body)))
    (xpElem "{jabber:client}iq"
         (xp5Tuple
              (xpAttr        "id"   xpPrim)
              (xpAttrImplied "from" xpPrim)
              (xpAttrImplied "to"   xpPrim)
              xpLangTag
              ((xpAttrFixed "type" "error"))
         )
         (xp2Tuple xpStanzaError (xpOption xpElemVerbatim))
    )

xpStreamError :: PU [Node] StreamErrorInfo
xpStreamError = xpWrap
    (\((cond,() ,()), txt, el) -> StreamErrorInfo cond txt el)
    (\(StreamErrorInfo cond txt el) ->((cond,() ,()), txt, el))
    (xpElemNodes
         (Name
              "error"
              (Just "http://etherx.jabber.org/streams")
              (Just "stream")
         )
         (xp3Tuple
              (xpElemByNamespace
                   "urn:ietf:params:xml:ns:xmpp-streams"
                   xpPrim
                   xpUnit
                   xpUnit
              )
              (xpOption $ xpElem
                   "{urn:ietf:params:xml:ns:xmpp-streams}text"
                   xpLangTag
                   (xpContent xpId)
              )
              (xpOption xpElemVerbatim) -- Application specific error conditions
         )
    )

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpPrim

xmlLang :: Name
xmlLang = Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

-- Given a pickler and an object, produces an Element.
pickleElem :: PU [Node] a -> a -> Element
pickleElem p = pickle $ xpNodeElem p

-- Given a pickler and an element, produces an object.
unpickleElem :: PU [Node] a -> Element -> Either UnpickleError a
unpickleElem p x = unpickle (xpNodeElem p) x

xpNodeElem :: PU [Node] a -> PU Element a
xpNodeElem xp = PU { pickleTree = \x -> Prelude.head $ (pickleTree xp x) >>= \y ->
                      case y of
                        NodeElement e -> [e]
                        _ -> []
             , unpickleTree = \x -> case unpickleTree xp $ [NodeElement x] of
                        Left l -> Left l
                        Right (a,(_,c)) -> Right (a,(Nothing,c))
                   }

mbl :: Maybe [a] -> [a]
mbl (Just l) = l
mbl Nothing = []

lmb :: [t] -> Maybe [t]
lmb [] = Nothing
lmb x = Just x

xpStream :: PU [Node] (Text, Maybe Jid, Maybe Jid, Maybe Text, Maybe LangTag)
xpStream = xpElemAttrs
    (Name "stream" (Just "http://etherx.jabber.org/streams") (Just "stream"))
    (xp5Tuple
         (xpAttr "version" xpId)
         (xpAttrImplied "from" xpPrim)
         (xpAttrImplied "to" xpPrim)
         (xpAttrImplied "id" xpId)
         xpLangTag
    )

-- Pickler/Unpickler for the stream features - TLS, SASL, and the rest.
xpStreamFeatures :: PU [Node] ServerFeatures
xpStreamFeatures = xpWrap
    (\(tls, sasl, rest) -> SF tls (mbl sasl) rest)
    (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
    (xpElemNodes
         (Name
             "features"
             (Just "http://etherx.jabber.org/streams")
             (Just "stream")
         )
         (xpTriple
              (xpOption pickleTlsFeature)
              (xpOption pickleSaslFeature)
              (xpAll xpElemVerbatim)
         )
    )
  where
    pickleTlsFeature :: PU [Node] Bool
    pickleTlsFeature = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
        (xpElemExists "required")
    pickleSaslFeature :: PU [Node] [Text]
    pickleSaslFeature =  xpElemNodes
        "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
        (xpAll $ xpElemNodes
             "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism" (xpContent xpId))
