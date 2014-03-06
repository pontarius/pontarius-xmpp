{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Van Laarhoven lenses for XMPP types. The lenses are designed to work with
-- the lens library. This module also provides a few simple accessors ('view',
-- 'modify', 'set' and 'getAll') so you don't need to pull in the
-- lens library to get some use out of them.
--
-- The name of the lenses corresponds to the field name of the data types with
-- an upper-case L appended. For documentation of the fields refer to the documentation of the data types (linked in the section header)

module Network.Xmpp.Lens
       ( Lens
       , Traversal
         -- * Accessors
         -- | Reimplementation of the basic lens functions so you don't have to
         -- bring in all of lens library in to use the lenses

         -- ** Lenses
       , view
       , modify
       , set
         -- * Traversals
       , getAll
         -- * Lenses

         -- ** Stanzas
       , IsStanza(..)
       , HasStanzaPayload(..)
       , IsErrorStanza(..)
       , messageTypeL
       , presenceTypeL
       , iqRequestTypeL
         -- *** 'StanzaError'
       , stanzaErrorTypeL
       , stanzaErrorConditionL
       , stanzaErrorTextL
       , stanzaErrorApplL
         -- *** 'StreamConfiguration'
       , preferredLangL
       , toJidL
       , connectionDetailsL
       , resolvConfL
       , establishSessionL
       , tlsBehaviourL
       , tlsParamsL
         -- *** 'SessionConfiguration'
       , streamConfigurationL
       , onConnectionClosedL
       , sessionStanzaIDsL
       , ensableRosterL
       , pluginsL
         -- ** IM
         -- *** Roster
         -- **** 'Roster'
       , verL
       , itemsL
         -- **** 'Item'
       , riApprovedL
       , riAskL
       , riJidL
       , riNameL
       , riSubscriptionL
       , riGroupsL
         -- **** 'QueryItem'
       , qiApprovedL
       , qiAskL
       , qiJidL
       , qiNameL
       , qiSubscriptionL
       , qiGroupsL
         -- **** 'Query'
       , queryVerL
       , queryItemsL
         -- ** IM Message
         -- *** 'MessageBody'
       , bodyLangL
       , bodyContentL
         -- *** 'MessageThread'
       , threadIdL
       , threadParentL
         -- *** 'MessageSubject'
       , subjectLangL
       , subjectContentL
         -- *** 'InstantMessage'
       , imThreadL
       , imSubjectL
       , imBodyL
         -- ** 'IMPresence'
       , showStatusL
       , statusL
       , priorityL

       )
       where

import           Control.Applicative
import           Data.Functor.Identity(Identity(..))
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text(Text)
import           Data.XML.Types(Element)
import           Network.DNS(ResolvConf)
import           Network.TLS (ClientParams)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.IM.Message
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.Types

-- | Van-Laarhoven lenses.
type Lens a b = Functor f => (b -> f b) -> a -> f a

type Traversal a b = Applicative f => (b -> f b) -> a -> f a


-- Accessors
---------------


-- | Read the value the lens is pointing to
view :: Lens a b -> a -> b
view l x = getConst $ l Const x

-- | Replace the value the lens is pointing to
set :: Lens a b -> b -> a -> a
set l b x = modify l (const b) x

-- | modify the Value(s) a Lens or Traversal is pointing to
modify :: Traversal a b -> (b -> b) -> a -> a
modify t f = runIdentity . t (Identity . f)

newtype Collect a b = Collect {getCollection :: [a]} deriving Functor

instance Applicative (Collect a) where
    pure _ = Collect []
    Collect xs <*> Collect ys = Collect $ xs ++ ys

-- | Return all the values a Traversal is pointing to in a list
getAll :: Traversal a b -> a -> [b]
getAll t = getCollection . t (Collect . pure)


-- Xmpp Lenses
--------------------

class IsStanza s where
    -- | From-attribute of the stanza
    from :: Lens s (Maybe Jid)
    -- | To-attribute of the stanza
    to   :: Lens s (Maybe Jid)
    -- | Langtag of the stanza
    lang :: Lens s (Maybe LangTag)
    -- | Stanza ID. Setting this to /Nothing/ for IQ* stanzas will set the id to
    -- the empty Text.
    sid :: Lens s (Maybe Text)
    -- | Traversal over the payload elements.
    payloadT :: Traversal s Element

traverseList :: Traversal [a] a
traverseList _inj [] = pure []
traverseList inj  (x:xs) = (:) <$> inj x <*> traverseList inj xs

instance IsStanza Message where
    from inj m@(Message{messageFrom=f}) = (\f' -> m{messageFrom = f'}) <$> inj f
    to inj m@(Message{messageTo=t}) = (\t' -> m{messageTo = t'}) <$> inj t
    lang inj m@(Message{messageLangTag=t}) =
        (\t' -> m{messageLangTag = t'}) <$> inj t
    sid inj m@(Message{messageID = i}) =
        ((\i' -> m{messageID = i'}) <$> inj i)
    payloadT inj m@(Message{messagePayload=pl}) =
        (\pl' -> m{messagePayload=pl'}) <$> traverseList inj pl


instance IsStanza MessageError where
    from inj m@(MessageError{messageErrorFrom=f}) =
        (\f' -> m{messageErrorFrom = f'}) <$> inj f
    to inj m@(MessageError{messageErrorTo=t}) =
        (\t' -> m{messageErrorTo = t'}) <$> inj t
    lang inj m@(MessageError{messageErrorLangTag=t}) =
        (\t' -> m{messageErrorLangTag = t'}) <$> inj t
    sid inj m@(MessageError{messageErrorID = i}) =
        ((\i' -> m{messageErrorID = i'}) <$> inj i)
    payloadT inj m@(MessageError{messageErrorPayload=pl}) =
        (\pl' -> m{messageErrorPayload=pl'}) <$> traverseList inj pl

instance IsStanza Presence where
    from inj m@(Presence{presenceFrom=f}) = (\f' -> m{presenceFrom = f'}) <$> inj f
    to inj m@(Presence{presenceTo=t}) = (\t' -> m{presenceTo = t'}) <$> inj t
    lang inj m@(Presence{presenceLangTag=t}) =
        (\t' -> m{presenceLangTag = t'}) <$> inj t
    sid inj m@(Presence{presenceID = i}) =
        ((\i' -> m{presenceID = i'}) <$> inj i)
    payloadT inj m@(Presence{presencePayload=pl}) =
        (\pl' -> m{presencePayload=pl'}) <$> traverseList inj pl

instance IsStanza PresenceError where
    from inj m@(PresenceError{presenceErrorFrom=f}) =
        (\f' -> m{presenceErrorFrom = f'}) <$> inj f
    to inj m@(PresenceError{presenceErrorTo=t}) =
        (\t' -> m{presenceErrorTo = t'}) <$> inj t
    lang inj m@(PresenceError{presenceErrorLangTag=t}) =
        (\t' -> m{presenceErrorLangTag = t'}) <$> inj t
    sid inj m@(PresenceError{presenceErrorID = i}) =
        ((\i' -> m{presenceErrorID = i'}) <$> inj i)
    payloadT inj m@(PresenceError{presenceErrorPayload=pl}) =
        (\pl' -> m{presenceErrorPayload=pl'}) <$> traverseList inj pl

instance IsStanza IQRequest where
    from inj m@(IQRequest{iqRequestFrom=f}) =
        (\f' -> m{iqRequestFrom = f'}) <$> inj f
    to inj m@(IQRequest{iqRequestTo=t}) =
        (\t' -> m{iqRequestTo = t'}) <$> inj t
    lang inj m@(IQRequest{iqRequestLangTag=t}) =
        (\t' -> m{iqRequestLangTag = t'}) <$> inj t
    sid inj m@(IQRequest{iqRequestID = i}) =
        ((\i' -> m{iqRequestID = i'}) <$> maybeNonempty inj i)
    payloadT inj m@(IQRequest{iqRequestPayload=pl}) =
        (\pl' -> m{iqRequestPayload=pl'}) <$> inj pl

instance IsStanza IQResult where
    from inj m@(IQResult{iqResultFrom=f}) =
        (\f' -> m{iqResultFrom = f'}) <$> inj f
    to inj m@(IQResult{iqResultTo=t}) =
        (\t' -> m{iqResultTo = t'}) <$> inj t
    lang inj m@(IQResult{iqResultLangTag=t}) =
        (\t' -> m{iqResultLangTag = t'}) <$> inj t
    sid inj m@(IQResult{iqResultID = i}) =
        ((\i' -> m{iqResultID = i'}) <$> maybeNonempty inj i)
    payloadT inj m@(IQResult{iqResultPayload=pl}) =
        (\pl' -> m{iqResultPayload=pl'}) <$> maybe (pure Nothing)
                                                   (fmap Just . inj) pl

instance IsStanza IQError where
    from inj m@(IQError{iqErrorFrom=f}) =
        (\f' -> m{iqErrorFrom = f'}) <$> inj f
    to inj m@(IQError{iqErrorTo=t}) =
        (\t' -> m{iqErrorTo = t'}) <$> inj t
    lang inj m@(IQError{iqErrorLangTag=t}) =
        (\t' -> m{iqErrorLangTag = t'}) <$> inj t
    sid inj m@(IQError{iqErrorID = i}) =
        ((\i' -> m{iqErrorID = i'}) <$> maybeNonempty inj i)
    payloadT inj m@(IQError{iqErrorPayload=pl}) =
        (\pl' -> m{iqErrorPayload=pl'}) <$> maybe (pure Nothing)
                                                  (fmap Just . inj) pl

liftLens :: (forall s. IsStanza s => Lens s a) -> Lens Stanza a
liftLens f inj (IQRequestS     s) = IQRequestS     <$> f inj s
liftLens f inj (IQResultS      s) = IQResultS      <$> f inj s
liftLens f inj (IQErrorS       s) = IQErrorS       <$> f inj s
liftLens f inj (MessageS       s) = MessageS       <$> f inj s
liftLens f inj (MessageErrorS  s) = MessageErrorS  <$> f inj s
liftLens f inj (PresenceS      s) = PresenceS      <$> f inj s
liftLens f inj (PresenceErrorS s) = PresenceErrorS <$> f inj s

liftTraversal :: (forall s. IsStanza s => Traversal s a) -> Traversal Stanza a
liftTraversal f inj (IQRequestS     s) = IQRequestS     <$> f inj s
liftTraversal f inj (IQResultS      s) = IQResultS      <$> f inj s
liftTraversal f inj (IQErrorS       s) = IQErrorS       <$> f inj s
liftTraversal f inj (MessageS       s) = MessageS       <$> f inj s
liftTraversal f inj (MessageErrorS  s) = MessageErrorS  <$> f inj s
liftTraversal f inj (PresenceS      s) = PresenceS      <$> f inj s
liftTraversal f inj (PresenceErrorS s) = PresenceErrorS <$> f inj s

instance IsStanza Stanza where
    from     = liftLens from
    to       = liftLens to
    lang     = liftLens lang
    sid      = liftLens sid
    payloadT = liftTraversal payloadT

maybeNonempty :: Lens Text (Maybe Text)
maybeNonempty inj x = (maybe Text.empty id)
                      <$> inj (if Text.null x then Nothing else Just x)


class IsErrorStanza s where
    -- | Error element of the stanza
    stanzaError :: Lens s StanzaError

instance IsErrorStanza IQError where
    stanzaError inj m@IQError{iqErrorStanzaError = i} =
        (\i' -> m{iqErrorStanzaError = i'}) <$> inj i

instance IsErrorStanza MessageError where
    stanzaError inj m@MessageError{messageErrorStanzaError = i} =
        (\i' -> m{messageErrorStanzaError = i'}) <$> inj i

instance IsErrorStanza PresenceError where
    stanzaError inj m@PresenceError{presenceErrorStanzaError = i} =
        (\i' -> m{presenceErrorStanzaError = i'}) <$> inj i

class HasStanzaPayload s p | s -> p where
    -- | Payload element(s) of the stanza. Since the amount of elements possible
    -- in a stanza vary by type, this lens can't be used with a general
    -- 'Stanza'. There is, however, a more general Traversable that works with
    -- all stanzas (including 'Stanza'): 'payloadT'
    payload :: Lens s p

instance HasStanzaPayload IQRequest Element where
    payload inj m@IQRequest{iqRequestPayload = i} =
        (\i' -> m{iqRequestPayload = i'}) <$> inj i

instance HasStanzaPayload IQResult (Maybe Element) where
    payload inj m@IQResult{iqResultPayload = i} =
        (\i' -> m{iqResultPayload = i'}) <$> inj i

instance HasStanzaPayload IQError (Maybe Element) where
    payload inj m@IQError{iqErrorPayload = i} =
        (\i' -> m{iqErrorPayload = i'}) <$> inj i

instance HasStanzaPayload Message [Element] where
    payload inj m@Message{messagePayload = i} =
        (\i' -> m{messagePayload = i'}) <$> inj i

instance HasStanzaPayload MessageError [Element] where
    payload inj m@MessageError{messageErrorPayload = i} =
        (\i' -> m{messageErrorPayload = i'}) <$> inj i

instance HasStanzaPayload Presence [Element] where
    payload inj m@Presence{presencePayload = i} =
        (\i' -> m{presencePayload = i'}) <$> inj i

instance HasStanzaPayload PresenceError [Element] where
    payload inj m@PresenceError{presenceErrorPayload = i} =
        (\i' -> m{presenceErrorPayload = i'}) <$> inj i

iqRequestTypeL :: Lens IQRequest IQRequestType
iqRequestTypeL inj p@IQRequest{iqRequestType = tp} =
    (\tp' -> p{iqRequestType = tp'}) <$> inj tp


messageTypeL :: Lens Message MessageType
messageTypeL inj p@Message{messageType = tp} =
    (\tp' -> p{messageType = tp'}) <$> inj tp

presenceTypeL :: Lens Presence PresenceType
presenceTypeL inj p@Presence{presenceType = tp} =
    (\tp' -> p{presenceType = tp'}) <$> inj tp


-- StanzaError
-----------------------

stanzaErrorTypeL :: Lens StanzaError StanzaErrorType
stanzaErrorTypeL inj se@StanzaError{stanzaErrorType = x} =
    (\x' -> se{stanzaErrorType = x'}) <$> inj x

stanzaErrorConditionL :: Lens StanzaError StanzaErrorCondition
stanzaErrorConditionL inj se@StanzaError{stanzaErrorCondition = x} =
    (\x' -> se{stanzaErrorCondition = x'}) <$> inj x

stanzaErrorTextL :: Lens StanzaError (Maybe (Maybe LangTag, NonemptyText))
stanzaErrorTextL inj se@StanzaError{stanzaErrorText = x} =
    (\x' -> se{stanzaErrorText = x'}) <$> inj x

stanzaErrorApplL  :: Lens StanzaError (Maybe Element)
stanzaErrorApplL inj se@StanzaError{stanzaErrorApplicationSpecificCondition = x} =
    (\x' -> se{stanzaErrorApplicationSpecificCondition = x'}) <$> inj x


-- StreamConfiguration
-----------------------

preferredLangL :: Lens StreamConfiguration (Maybe LangTag)
preferredLangL inj sc@StreamConfiguration{preferredLang = x}
    = (\x' -> sc{preferredLang = x'}) <$> inj x

toJidL :: Lens StreamConfiguration (Maybe (Jid, Bool))
toJidL inj sc@StreamConfiguration{toJid = x}
    = (\x' -> sc{toJid = x'}) <$> inj x

connectionDetailsL :: Lens StreamConfiguration ConnectionDetails
connectionDetailsL inj sc@StreamConfiguration{connectionDetails = x}
    = (\x' -> sc{connectionDetails = x'}) <$> inj x

resolvConfL :: Lens StreamConfiguration ResolvConf
resolvConfL inj sc@StreamConfiguration{resolvConf = x}
    = (\x' -> sc{resolvConf = x'}) <$> inj x

establishSessionL :: Lens StreamConfiguration Bool
establishSessionL inj sc@StreamConfiguration{establishSession = x}
    = (\x' -> sc{establishSession = x'}) <$> inj x

tlsBehaviourL :: Lens StreamConfiguration TlsBehaviour
tlsBehaviourL inj sc@StreamConfiguration{tlsBehaviour = x}
    = (\x' -> sc{tlsBehaviour = x'}) <$> inj x

tlsParamsL :: Lens StreamConfiguration ClientParams
tlsParamsL inj sc@StreamConfiguration{tlsParams = x}
    = (\x' -> sc{tlsParams = x'}) <$> inj x

-- SessioConfiguration
-----------------------
streamConfigurationL :: Lens SessionConfiguration StreamConfiguration
streamConfigurationL inj sc@SessionConfiguration{sessionStreamConfiguration = x}
    = (\x' -> sc{sessionStreamConfiguration = x'}) <$> inj x

onConnectionClosedL :: Lens SessionConfiguration (Session -> XmppFailure -> IO ())
onConnectionClosedL inj sc@SessionConfiguration{onConnectionClosed = x}
    = (\x' -> sc{onConnectionClosed = x'}) <$> inj x

sessionStanzaIDsL :: Lens SessionConfiguration (IO (IO Text))
sessionStanzaIDsL inj sc@SessionConfiguration{sessionStanzaIDs = x}
    = (\x' -> sc{sessionStanzaIDs = x'}) <$> inj x

ensableRosterL :: Lens SessionConfiguration Bool
ensableRosterL inj sc@SessionConfiguration{enableRoster = x}
    = (\x' -> sc{enableRoster = x'}) <$> inj x

pluginsL :: Lens SessionConfiguration [Plugin]
pluginsL inj sc@SessionConfiguration{plugins = x}
    = (\x' -> sc{plugins = x'}) <$> inj x

-- Roster
------------------

verL :: Lens Roster (Maybe Text)
verL inj r@Roster{ver = x} = (\x' -> r{ver = x'}) <$> inj x

itemsL :: Lens Roster (Map.Map Jid Item)
itemsL inj r@Roster{items = x} = (\x' -> r{items = x'}) <$> inj x

-- Item
----------------------

riApprovedL :: Lens Item Bool
riApprovedL inj i@Item{riApproved = x} = (\x' -> i{riApproved = x'}) <$> inj x

riAskL :: Lens Item Bool
riAskL inj i@Item{riAsk = x} = (\x' -> i{riAsk = x'}) <$> inj x

riJidL :: Lens Item Jid
riJidL inj i@Item{riJid = x} = (\x' -> i{riJid = x'}) <$> inj x

riNameL :: Lens Item (Maybe Text)
riNameL inj i@Item{riName = x} = (\x' -> i{riName = x'}) <$> inj x

riSubscriptionL :: Lens Item Subscription
riSubscriptionL inj i@Item{riSubscription = x} =
    (\x' -> i{riSubscription = x'}) <$> inj x

riGroupsL :: Lens Item [Text]
riGroupsL inj i@Item{riGroups = x} = (\x' -> i{riGroups = x'}) <$> inj x


-- QueryItem
-------------------
qiApprovedL :: Lens QueryItem (Maybe Bool)
qiApprovedL inj i@QueryItem{qiApproved = x} =
    (\x' -> i{qiApproved = x'}) <$> inj x

qiAskL :: Lens QueryItem Bool
qiAskL inj i@QueryItem{qiAsk = x} = (\x' -> i{qiAsk = x'}) <$> inj x

qiJidL :: Lens QueryItem Jid
qiJidL inj i@QueryItem{qiJid = x} = (\x' -> i{qiJid = x'}) <$> inj x

qiNameL :: Lens QueryItem (Maybe Text)
qiNameL inj i@QueryItem{qiName = x} = (\x' -> i{qiName = x'}) <$> inj x

qiSubscriptionL :: Lens QueryItem (Maybe Subscription)
qiSubscriptionL inj i@QueryItem{qiSubscription = x} =
    (\x' -> i{qiSubscription = x'}) <$> inj x

qiGroupsL :: Lens QueryItem [Text]
qiGroupsL inj i@QueryItem{qiGroups = x} = (\x' -> i{qiGroups = x'}) <$> inj x

queryVerL :: Lens Query (Maybe Text)
queryVerL inj i@Query{queryVer = x} = (\x' -> i{queryVer = x'}) <$> inj x

queryItemsL :: Lens Query [QueryItem]
queryItemsL inj i@Query{queryItems = x} = (\x' -> i{queryItems = x'}) <$> inj x


-- IM
-------------------


bodyLangL :: Lens MessageBody (Maybe LangTag)
bodyLangL inj m@MessageBody{bodyLang = bl} = (\bl' -> m{bodyLang = bl'}) <$> inj bl

bodyContentL :: Lens MessageBody Text
bodyContentL inj m@MessageBody{bodyContent = bc} =
    (\bc' -> m{bodyContent = bc'}) <$> inj bc

threadIdL :: Lens MessageThread Text
threadIdL inj m@MessageThread{threadID = bc} =
    (\bc' -> m{threadID = bc'}) <$> inj bc

threadParentL :: Lens MessageThread (Maybe Text)
threadParentL inj m@MessageThread{threadParent = bc} =
    (\bc' -> m{threadParent = bc'}) <$> inj bc

subjectLangL :: Lens MessageSubject (Maybe LangTag)
subjectLangL inj m@MessageSubject{subjectLang = bc} =
    (\bc' -> m{subjectLang = bc'}) <$> inj bc

subjectContentL :: Lens MessageSubject Text
subjectContentL inj m@MessageSubject{subjectContent = bc} =
    (\bc' -> m{subjectContent = bc'}) <$> inj bc

imThreadL :: Lens InstantMessage (Maybe MessageThread)
imThreadL inj m@InstantMessage{imThread = bc} =
    (\bc' -> m{imThread = bc'}) <$> inj bc

imSubjectL :: Lens InstantMessage [MessageSubject]
imSubjectL inj m@InstantMessage{imSubject = bc} =
    (\bc' -> m{imSubject = bc'}) <$> inj bc

imBodyL :: Lens InstantMessage [MessageBody]
imBodyL inj m@InstantMessage{imBody = bc} =
    (\bc' -> m{imBody = bc'}) <$> inj bc

-- IM Presence
------------------

showStatusL :: Lens IMPresence (Maybe ShowStatus)
showStatusL inj m@IMP{showStatus = bc} =
    (\bc' -> m{showStatus = bc'}) <$> inj bc

statusL :: Lens IMPresence (Maybe Text)
statusL inj m@IMP{status = bc} =
    (\bc' -> m{status = bc'}) <$> inj bc

priorityL :: Lens IMPresence (Maybe Int)
priorityL inj m@IMP{priority = bc} =
    (\bc' -> m{priority = bc'}) <$> inj bc
