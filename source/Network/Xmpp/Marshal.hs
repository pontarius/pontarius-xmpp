-- Picklers and unpicklers convert Haskell data to XML and XML to Haskell data,
-- respectively. By convention, pickler/unpickler ("PU") function names start
-- with "xp".

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Marshal where

import           Data.XML.Pickle
import           Data.XML.Types

import qualified Control.Exception as Ex
import           Data.Text (Text)
import qualified Data.Text as Text

import           Network.Xmpp.Types

xpNonemptyText :: PU Text NonemptyText
xpNonemptyText = ("xpNonemptyText" , "") <?+> xpWrap Nonempty fromNonempty xpText

xpStreamElement :: PU [Node] (Either StreamErrorInfo XmppElement)
xpStreamElement = xpEither xpStreamError $
                    xpWrap (\v -> case v of
                                   Left l -> XmppStanza l
                                   Right r -> XmppNonza r
                           )
                           ( \v -> case v of
                                    XmppStanza l -> Left l
                                    XmppNonza r -> Right r)
                      $ xpEither xpStanza xpElemVerbatim

xpStreamStanza :: PU [Node] (Either StreamErrorInfo Stanza)
xpStreamStanza = xpEither xpStreamError xpStanza

xpExtendedAttrs :: PU [Attribute] [ExtendedAttribute]
xpExtendedAttrs = ("xpAttrVerbatim" , "") <?+>
                    xpIso (map (\(name, cs) -> (name, flattenContents cs)))
                          (map (\(name, c) -> (name, [ContentText c])))
  where
    flattenContents = Text.concat . filterContentText
    filterContentText = map (\c -> case c of
        ContentText t -> t
        ContentEntity{} -> Ex.throw UnresolvedEntityException )

xpStanza :: PU [Node] Stanza
xpStanza = ("xpStanza" , "") <?+> xpAlt stanzaSel
    [ xpWrap IQRequestS     (\(IQRequestS     x) -> x) xpIQRequest
    , xpWrap IQResultS      (\(IQResultS      x) -> x) xpIQResult
    , xpWrap IQErrorS       (\(IQErrorS       x) -> x) xpIQError
    , xpWrap MessageErrorS  (\(MessageErrorS  x) -> x) xpMessageError
    , xpWrap MessageS       (\(MessageS       x) -> x) xpMessage
    , xpWrap PresenceErrorS (\(PresenceErrorS x) -> x) xpPresenceError
    , xpWrap PresenceS      (\(PresenceS      x) -> x) xpPresence
    ]
  where
    -- Selector for which pickler to execute above.
    stanzaSel :: Stanza -> Int
    stanzaSel (IQRequestS     _) = 0
    stanzaSel (IQResultS      _) = 1
    stanzaSel (IQErrorS       _) = 2
    stanzaSel (MessageErrorS  _) = 3
    stanzaSel (MessageS       _) = 4
    stanzaSel (PresenceErrorS _) = 5
    stanzaSel (PresenceS      _) = 6

xpMessage :: PU [Node] (Message)
xpMessage = ("xpMessage" , "") <?+> xpWrap
    (\((tp, qid, from, to, lang, attrs), ext) -> Message qid from to lang tp ext attrs)
    (\(Message qid from to lang tp ext attrs) -> ((tp, qid, from, to, lang, attrs), ext))
    (xpElem "{jabber:client}message"
         (xp6Tuple
             (xpDefault Normal $ xpAttr "type" xpMessageType)
             (xpAttrImplied "id"   xpId)
             (xpAttrImplied "from" xpJid)
             (xpAttrImplied "to"   xpJid)
             xpLangTag
             xpExtendedAttrs
             -- TODO: NS?
         )
         (xpAll xpElemVerbatim)
    )

xpPresence :: PU [Node] Presence
xpPresence = ("xpPresence" , "") <?+> xpWrap
    (\((qid, from, to, lang, tp, attr), ext)
        -> Presence qid from to lang tp ext attr)
    (\(Presence qid from to lang tp ext attr)
       -> ((qid, from, to, lang, tp, attr), ext))
    (xpElem "{jabber:client}presence"
         (xp6Tuple
              (xpAttrImplied "id"   xpId)
              (xpAttrImplied "from" xpJid)
              (xpAttrImplied "to"   xpJid)
              xpLangTag
              (xpDefault Available $ xpAttr "type" xpPresenceType)
              xpExtendedAttrs
         )
         (xpAll xpElemVerbatim)
    )

xpIQRequest :: PU [Node] IQRequest
xpIQRequest = ("xpIQRequest" , "") <?+> xpWrap
    (\((qid, from, to, lang, tp, attr),body)
       -> IQRequest qid from to lang tp body attr)
    (\(IQRequest qid from to lang tp body attr)
        -> ((qid, from, to, lang, tp, attr), body))
    (xpElem "{jabber:client}iq"
         (xp6Tuple
             (xpAttr        "id"   xpId)
             (xpAttrImplied "from" xpJid)
             (xpAttrImplied "to"   xpJid)
             xpLangTag
             ((xpAttr        "type" xpIQRequestType))
             xpExtendedAttrs
         )
         xpElemVerbatim
    )

xpIQResult :: PU [Node] IQResult
xpIQResult = ("xpIQResult" , "") <?+> xpWrap
    (\((qid, from, to, lang, _tp, attr),body)
        -> IQResult qid from to lang body attr)
    (\(IQResult qid from to lang body attr)
        -> ((qid, from, to, lang, (), attr ), body))
    (xpElem "{jabber:client}iq"
         (xp6Tuple
             (xpAttr        "id"   xpId)
             (xpAttrImplied "from" xpJid)
             (xpAttrImplied "to"   xpJid)
             xpLangTag
             ((xpAttrFixed "type" "result"))
             xpExtendedAttrs
         )
         (xpOption xpElemVerbatim)
    )

----------------------------------------------------------
-- Errors
----------------------------------------------------------

xpStanzaErrorCondition :: PU [Node] StanzaErrorCondition
xpStanzaErrorCondition = ("xpErrorCondition" , "") <?+> xpWrapEither
                   (\(cond, (),cont) -> case (cond, cont) of
                         (Gone _, x) -> Right $ Gone x
                         (Redirect _, x) -> Right $ Redirect x
                         (x , Nothing) -> Right x
                         _ -> Left
                              ("Only Gone and Redirect may have character data"
                                 :: String)
                              )
                   (\x -> case x of
                         (Gone t) -> (Gone Nothing, (),  t)
                         (Redirect t) -> (Redirect Nothing, () , t)
                         c -> (c, (), Nothing))
    (xpElemByNamespace
        "urn:ietf:params:xml:ns:xmpp-stanzas"
        xpStanzaErrorConditionShape
        xpUnit
        (xpOption $ xpContent xpNonemptyText)
    )
  where
    -- Create the "shape" of the error condition. In case of Gone and Redirect
    -- the optional field is left empty and must be filled in by the caller
    xpStanzaErrorConditionShape :: PU Text StanzaErrorCondition
    xpStanzaErrorConditionShape = ("xpStanzaErrorCondition", "") <?>
            xpIso stanzaErrorConditionFromText
                  stanzaErrorConditionToText
    stanzaErrorConditionToText BadRequest = "bad-request"
    stanzaErrorConditionToText Conflict = "conflict"
    stanzaErrorConditionToText FeatureNotImplemented = "feature-not-implemented"
    stanzaErrorConditionToText Forbidden = "forbidden"
    stanzaErrorConditionToText (Gone _) = "gone"
    stanzaErrorConditionToText InternalServerError = "internal-server-error"
    stanzaErrorConditionToText ItemNotFound = "item-not-found"
    stanzaErrorConditionToText JidMalformed = "jid-malformed"
    stanzaErrorConditionToText NotAcceptable = "not-acceptable"
    stanzaErrorConditionToText NotAllowed = "not-allowed"
    stanzaErrorConditionToText NotAuthorized = "not-authorized"
    stanzaErrorConditionToText PolicyViolation = "policy-violation"
    stanzaErrorConditionToText RecipientUnavailable = "recipient-unavailable"
    stanzaErrorConditionToText (Redirect _) = "redirect"
    stanzaErrorConditionToText RegistrationRequired = "registration-required"
    stanzaErrorConditionToText RemoteServerNotFound = "remote-server-not-found"
    stanzaErrorConditionToText RemoteServerTimeout = "remote-server-timeout"
    stanzaErrorConditionToText ResourceConstraint = "resource-constraint"
    stanzaErrorConditionToText ServiceUnavailable = "service-unavailable"
    stanzaErrorConditionToText SubscriptionRequired = "subscription-required"
    stanzaErrorConditionToText UndefinedCondition = "undefined-condition"
    stanzaErrorConditionToText UnexpectedRequest = "unexpected-request"
    stanzaErrorConditionFromText "bad-request" = BadRequest
    stanzaErrorConditionFromText "conflict" = Conflict
    stanzaErrorConditionFromText "feature-not-implemented" = FeatureNotImplemented
    stanzaErrorConditionFromText "forbidden" = Forbidden
    stanzaErrorConditionFromText "gone" = Gone Nothing
    stanzaErrorConditionFromText "internal-server-error" = InternalServerError
    stanzaErrorConditionFromText "item-not-found" = ItemNotFound
    stanzaErrorConditionFromText "jid-malformed" = JidMalformed
    stanzaErrorConditionFromText "not-acceptable" = NotAcceptable
    stanzaErrorConditionFromText "not-allowed" = NotAllowed
    stanzaErrorConditionFromText "not-authorized" = NotAuthorized
    stanzaErrorConditionFromText "policy-violation" = PolicyViolation
    stanzaErrorConditionFromText "recipient-unavailable" = RecipientUnavailable
    stanzaErrorConditionFromText "redirect" = Redirect Nothing
    stanzaErrorConditionFromText "registration-required" = RegistrationRequired
    stanzaErrorConditionFromText "remote-server-not-found" = RemoteServerNotFound
    stanzaErrorConditionFromText "remote-server-timeout" = RemoteServerTimeout
    stanzaErrorConditionFromText "resource-constraint" = ResourceConstraint
    stanzaErrorConditionFromText "service-unavailable" = ServiceUnavailable
    stanzaErrorConditionFromText "subscription-required" = SubscriptionRequired
    stanzaErrorConditionFromText "undefined-condition" = UndefinedCondition
    stanzaErrorConditionFromText "unexpected-request" = UnexpectedRequest
    stanzaErrorConditionFromText _ = UndefinedCondition



xpStanzaError :: PU [Node] StanzaError
xpStanzaError = ("xpStanzaError" , "") <?+> xpWrap
    (\((tp, _code), (cond, txt, ext)) -> StanzaError tp cond txt ext)
    (\(StanzaError tp cond txt ext) -> ((tp, Nothing), (cond, txt, ext)))
    (xpElem "{jabber:client}error"
         (xp2Tuple
             (xpAttr "type" xpStanzaErrorType)
             (xpAttribute' "code" xpId))
         (xp3Tuple
              xpStanzaErrorCondition
              (xpOption $ xpElem "{urn:ietf:params:xml:ns:xmpp-stanzas}text"
                   (xpAttrImplied xmlLang xpLang)
                   (xpContent xpNonemptyText)
              )
              (xpOption xpElemVerbatim)
         )
    )

xpMessageError :: PU [Node] (MessageError)
xpMessageError = ("xpMessageError" , "") <?+> xpWrap
    (\((_, qid, from, to, lang, attr), (err, ext)) ->
        MessageError qid from to lang err ext attr)
    (\(MessageError qid from to lang err ext attr) ->
        (((), qid, from, to, lang, attr), (err, ext)))
    (xpElem "{jabber:client}message"
         (xp6Tuple
              (xpAttrFixed   "type" "error")
              (xpAttrImplied "id"   xpId)
              (xpAttrImplied "from" xpJid)
              (xpAttrImplied "to"   xpJid)
              (xpAttrImplied xmlLang xpLang)
              xpExtendedAttrs
         )
         (xp2Tuple xpStanzaError (xpAll xpElemVerbatim))
    )

xpPresenceError :: PU [Node] PresenceError
xpPresenceError = ("xpPresenceError" , "") <?+> xpWrap
    (\((qid, from, to, lang, _, attr),(err, ext)) ->
        PresenceError qid from to lang err ext attr)
    (\(PresenceError qid from to lang err ext attr) ->
        ((qid, from, to, lang, (), attr), (err, ext)))
    (xpElem "{jabber:client}presence"
         (xp6Tuple
              (xpAttrImplied "id"   xpId)
              (xpAttrImplied "from" xpJid)
              (xpAttrImplied "to"   xpJid)
              xpLangTag
              (xpAttrFixed "type" "error")
              xpExtendedAttrs
         )
         (xp2Tuple xpStanzaError (xpAll xpElemVerbatim))
    )

xpIQError :: PU [Node] IQError
xpIQError = ("xpIQError" , "") <?+> xpWrap
    (\((qid, from, to, lang, _tp, attr),(err, body)) ->
        IQError qid from to lang err body attr)
    (\(IQError qid from to lang err body attr) ->
        ((qid, from, to, lang, (), attr), (err, body)))
    (xpElem "{jabber:client}iq"
         (xp6Tuple
              (xpAttr        "id"   xpId)
              (xpAttrImplied "from" xpJid)
              (xpAttrImplied "to"   xpJid)
              xpLangTag
              ((xpAttrFixed "type" "error"))
              xpExtendedAttrs
         )
         (xp2Tuple xpStanzaError (xpOption xpElemVerbatim))
    )

xpStreamError :: PU [Node] StreamErrorInfo
xpStreamError = ("xpStreamError" , "") <?+> xpWrap
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
                   xpStreamErrorCondition
                   xpUnit
                   xpUnit
              )
              (xpOption $ xpElem
                   "{urn:ietf:params:xml:ns:xmpp-streams}text"
                   xpLangTag
                   (xpContent xpNonemptyText)
              )
              (xpOption xpElemVerbatim) -- Application specific error conditions
         )
    )

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpLang

xpLang :: PU Text LangTag
xpLang = ("xpLang", "") <?>
    xpPartial ( \input -> case langTagFromText input of
                               Nothing -> Left "Could not parse language tag."
                               Just j -> Right j)
              langTagToText

xmlLang :: Name
xmlLang = Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

-- Given a pickler and an object, produces an Element.
pickleElem :: PU [Node] a -> a -> Element
pickleElem p = pickle $ xpNodeElem p

-- Given a pickler and an element, produces an object.
unpickleElem :: PU [Node] a -> Element -> Either UnpickleError a
unpickleElem p x = unpickle (xpNodeElem p) x

xpNodeElem :: PU [Node] a -> PU Element a
xpNodeElem = xpRoot . xpUnliftElems

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
         (xpAttrImplied "from" xpJid)
         (xpAttrImplied "to" xpJid)
         (xpAttrImplied "id" xpId)
         xpLangTag
    )

-- Pickler/Unpickler for the stream features - TLS, SASL, and the rest.
xpStreamFeatures :: PU [Node] StreamFeatures
xpStreamFeatures = ("xpStreamFeatures","") <?> xpWrap
    (\(tls, sasl, ver, preAppr, session, rest)
       -> StreamFeatures tls (mbl sasl) ver preAppr session rest )
    (\(StreamFeatures tls sasl ver preAppr session rest)
     -> (tls, lmb sasl, ver, preAppr, session, rest))
    (xpElemNodes
         (Name
             "features"
             (Just "http://etherx.jabber.org/streams")
             (Just "stream")
         )
         (xp6Tuple
              (xpOption pickleTlsFeature)
              (xpOption pickleSaslFeature)
              (xpOption pickleRosterVer)
              picklePreApproval
              (xpOption pickleSessionFeature)
              (xpAll xpElemVerbatim)
         )
    )
  where
    pickleTlsFeature :: PU [Node] Bool
    pickleTlsFeature = ("pickleTlsFeature", "") <?>
        xpElemNodes "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
        (xpElemExists "{urn:ietf:params:xml:ns:xmpp-tls}required")
    pickleSaslFeature :: PU [Node] [Text]
    pickleSaslFeature = ("pickleSaslFeature", "") <?>
        xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
        (xpAll $ xpElemNodes
             "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism" (xpContent xpId))
    pickleRosterVer = xpElemNodes "{urn:xmpp:features:rosterver}ver" $
                           xpElemExists "{urn:xmpp:features:rosterver}optional"
    picklePreApproval = xpElemExists "{urn:xmpp:features:pre-approval}sub"
    pickleSessionFeature :: PU [Node] Bool
    pickleSessionFeature = ("pickleSessionFeature", "") <?>
        xpElemNodes "{urn:ietf:params:xml:ns:xmpp-session}session"
        (xpElemExists "{urn:ietf:params:xml:ns:xmpp-session}optional")


xpJid :: PU Text Jid
xpJid = ("xpJid", "") <?>
        xpPartial ( \input -> case jidFromText input of
                                   Nothing -> Left "Could not parse JID."
                                   Just j -> Right j)
                  jidToText

xpIQRequestType :: PU Text IQRequestType
xpIQRequestType = ("xpIQRequestType", "") <?>
        xpPartial ( \input -> case iqRequestTypeFromText input of
                                   Nothing -> Left "Could not parse IQ request type."
                                   Just j -> Right j)
                  iqRequestTypeToText
  where
    iqRequestTypeFromText "get" = Just Get
    iqRequestTypeFromText "set" = Just Set
    iqRequestTypeFromText _ = Nothing
    iqRequestTypeToText Get = "get"
    iqRequestTypeToText Set = "set"

xpMessageType :: PU Text MessageType
xpMessageType = ("xpMessageType", "") <?>
        xpIso messageTypeFromText
              messageTypeToText
  where
    messageTypeFromText "chat" = Chat
    messageTypeFromText "groupchat" = GroupChat
    messageTypeFromText "headline" = Headline
    messageTypeFromText "normal" = Normal
    messageTypeFromText _ = Normal
    messageTypeToText Chat = "chat"
    messageTypeToText GroupChat = "groupchat"
    messageTypeToText Headline = "headline"
    messageTypeToText Normal = "normal"

xpPresenceType :: PU Text PresenceType
xpPresenceType = ("xpPresenceType", "") <?>
        xpPartial ( \input -> case presenceTypeFromText input of
                                   Nothing -> Left "Could not parse presence type."
                                   Just j -> Right j)
                  presenceTypeToText
  where
    presenceTypeFromText "" = Just Available
    presenceTypeFromText "available" = Just Available
    presenceTypeFromText "unavailable" = Just Unavailable
    presenceTypeFromText "subscribe" = Just Subscribe
    presenceTypeFromText "subscribed" = Just Subscribed
    presenceTypeFromText "unsubscribe" = Just Unsubscribe
    presenceTypeFromText "unsubscribed" = Just Unsubscribed
    presenceTypeFromText "probe" = Just Probe
    presenceTypeFromText _ = Nothing
    presenceTypeToText Available = "available"
    presenceTypeToText Unavailable = "unavailable"
    presenceTypeToText Subscribe = "subscribe"
    presenceTypeToText Subscribed = "subscribed"
    presenceTypeToText Unsubscribe = "unsubscribe"
    presenceTypeToText Unsubscribed = "unsubscribed"
    presenceTypeToText Probe = "probe"

xpStanzaErrorType :: PU Text StanzaErrorType
xpStanzaErrorType = ("xpStanzaErrorType", "") <?>
        xpPartial ( \input -> case stanzaErrorTypeFromText input of
                                   Nothing -> Left "Could not parse stanza error type."
                                   Just j -> Right j)
                  stanzaErrorTypeToText
  where
    stanzaErrorTypeFromText "auth" = Just Auth
    stanzaErrorTypeFromText "cancel" = Just Cancel
    stanzaErrorTypeFromText "continue" = Just Continue
    stanzaErrorTypeFromText "modify" = Just Modify
    stanzaErrorTypeFromText "wait" = Just Wait
    stanzaErrorTypeFromText _ = Nothing
    stanzaErrorTypeToText Auth = "auth"
    stanzaErrorTypeToText Cancel = "cancel"
    stanzaErrorTypeToText Continue = "continue"
    stanzaErrorTypeToText Modify = "modify"
    stanzaErrorTypeToText Wait = "wait"


xpStreamErrorCondition :: PU Text StreamErrorCondition
xpStreamErrorCondition = ("xpStreamErrorCondition", "") <?>
        xpIso streamErrorConditionFromText
              streamErrorConditionToText
  where
    streamErrorConditionToText StreamBadFormat              = "bad-format"
    streamErrorConditionToText StreamBadNamespacePrefix     = "bad-namespace-prefix"
    streamErrorConditionToText StreamConflict               = "conflict"
    streamErrorConditionToText StreamConnectionTimeout      = "connection-timeout"
    streamErrorConditionToText StreamHostGone               = "host-gone"
    streamErrorConditionToText StreamHostUnknown            = "host-unknown"
    streamErrorConditionToText StreamImproperAddressing     = "improper-addressing"
    streamErrorConditionToText StreamInternalServerError    = "internal-server-error"
    streamErrorConditionToText StreamInvalidFrom            = "invalid-from"
    streamErrorConditionToText StreamInvalidNamespace       = "invalid-namespace"
    streamErrorConditionToText StreamInvalidXml             = "invalid-xml"
    streamErrorConditionToText StreamNotAuthorized          = "not-authorized"
    streamErrorConditionToText StreamNotWellFormed          = "not-well-formed"
    streamErrorConditionToText StreamPolicyViolation        = "policy-violation"
    streamErrorConditionToText StreamRemoteConnectionFailed = "remote-connection-failed"
    streamErrorConditionToText StreamReset                  = "reset"
    streamErrorConditionToText StreamResourceConstraint     = "resource-constraint"
    streamErrorConditionToText StreamRestrictedXml          = "restricted-xml"
    streamErrorConditionToText StreamSeeOtherHost           = "see-other-host"
    streamErrorConditionToText StreamSystemShutdown         = "system-shutdown"
    streamErrorConditionToText StreamUndefinedCondition     = "undefined-condition"
    streamErrorConditionToText StreamUnsupportedEncoding    = "unsupported-encoding"
    streamErrorConditionToText StreamUnsupportedFeature     = "unsupported-feature"
    streamErrorConditionToText StreamUnsupportedStanzaType  = "unsupported-stanza-type"
    streamErrorConditionToText StreamUnsupportedVersion     = "unsupported-version"
    streamErrorConditionFromText "bad-format" = StreamBadFormat
    streamErrorConditionFromText "bad-namespace-prefix" = StreamBadNamespacePrefix
    streamErrorConditionFromText "conflict" = StreamConflict
    streamErrorConditionFromText "connection-timeout" = StreamConnectionTimeout
    streamErrorConditionFromText "host-gone" = StreamHostGone
    streamErrorConditionFromText "host-unknown" = StreamHostUnknown
    streamErrorConditionFromText "improper-addressing" = StreamImproperAddressing
    streamErrorConditionFromText "internal-server-error" = StreamInternalServerError
    streamErrorConditionFromText "invalid-from" = StreamInvalidFrom
    streamErrorConditionFromText "invalid-namespace" = StreamInvalidNamespace
    streamErrorConditionFromText "invalid-xml" = StreamInvalidXml
    streamErrorConditionFromText "not-authorized" = StreamNotAuthorized
    streamErrorConditionFromText "not-well-formed" = StreamNotWellFormed
    streamErrorConditionFromText "policy-violation" = StreamPolicyViolation
    streamErrorConditionFromText "remote-connection-failed" = StreamRemoteConnectionFailed
    streamErrorConditionFromText "reset" = StreamReset
    streamErrorConditionFromText "resource-constraint" = StreamResourceConstraint
    streamErrorConditionFromText "restricted-xml" = StreamRestrictedXml
    streamErrorConditionFromText "see-other-host" = StreamSeeOtherHost
    streamErrorConditionFromText "system-shutdown" = StreamSystemShutdown
    streamErrorConditionFromText "undefined-condition" = StreamUndefinedCondition
    streamErrorConditionFromText "unsupported-encoding" = StreamUnsupportedEncoding
    streamErrorConditionFromText "unsupported-feature" = StreamUnsupportedFeature
    streamErrorConditionFromText "unsupported-stanza-type" = StreamUnsupportedStanzaType
    streamErrorConditionFromText "unsupported-version" = StreamUnsupportedVersion
    streamErrorConditionFromText _ = StreamUndefinedCondition -- §4.9.2
