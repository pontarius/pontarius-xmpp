{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Network.Xmpp.Lens where

import Control.Applicative((<$>), Const(..))
import Network.Xmpp.Types

type Lens a b = Functor f => (b -> f b) -> a -> f a

class StanzaC s where
    from :: Lens s (Maybe Jid)
    to   :: Lens s (Maybe Jid)
    lang :: Lens s (Maybe LangTag)


instance StanzaC Message where
    from inj m@(Message{messageFrom=f}) = (\f' -> m{messageFrom = f'}) <$> inj f
    to inj m@(Message{messageTo=t}) = (\t' -> m{messageTo = t'}) <$> inj t
    lang inj m@(Message{messageLangTag=t}) =
        (\t' -> m{messageLangTag = t'}) <$> inj t


instance StanzaC MessageError where
    from inj m@(MessageError{messageErrorFrom=f}) =
        (\f' -> m{messageErrorFrom = f'}) <$> inj f
    to inj m@(MessageError{messageErrorTo=t}) =
        (\t' -> m{messageErrorTo = t'}) <$> inj t
    lang inj m@(MessageError{messageErrorLangTag=t}) =
        (\t' -> m{messageErrorLangTag = t'}) <$> inj t

instance StanzaC Presence where
    from inj m@(Presence{presenceFrom=f}) = (\f' -> m{presenceFrom = f'}) <$> inj f
    to inj m@(Presence{presenceTo=t}) = (\t' -> m{presenceTo = t'}) <$> inj t
    lang inj m@(Presence{presenceLangTag=t}) =
        (\t' -> m{presenceLangTag = t'}) <$> inj t

instance StanzaC PresenceError where
    from inj m@(PresenceError{presenceErrorFrom=f}) =
        (\f' -> m{presenceErrorFrom = f'}) <$> inj f
    to inj m@(PresenceError{presenceErrorTo=t}) =
        (\t' -> m{presenceErrorTo = t'}) <$> inj t
    lang inj m@(PresenceError{presenceErrorLangTag=t}) =
        (\t' -> m{presenceErrorLangTag = t'}) <$> inj t

instance StanzaC IQRequest where
    from inj m@(IQRequest{iqRequestFrom=f}) =
        (\f' -> m{iqRequestFrom = f'}) <$> inj f
    to inj m@(IQRequest{iqRequestTo=t}) =
        (\t' -> m{iqRequestTo = t'}) <$> inj t
    lang inj m@(IQRequest{iqRequestLangTag=t}) =
        (\t' -> m{iqRequestLangTag = t'}) <$> inj t

instance StanzaC IQResult where
    from inj m@(IQResult{iqResultFrom=f}) =
        (\f' -> m{iqResultFrom = f'}) <$> inj f
    to inj m@(IQResult{iqResultTo=t}) =
        (\t' -> m{iqResultTo = t'}) <$> inj t
    lang inj m@(IQResult{iqResultLangTag=t}) =
        (\t' -> m{iqResultLangTag = t'}) <$> inj t

instance StanzaC IQError where
    from inj m@(IQError{iqErrorFrom=f}) =
        (\f' -> m{iqErrorFrom = f'}) <$> inj f
    to inj m@(IQError{iqErrorTo=t}) =
        (\t' -> m{iqErrorTo = t'}) <$> inj t
    lang inj m@(IQError{iqErrorLangTag=t}) =
        (\t' -> m{iqErrorLangTag = t'}) <$> inj t

lift :: (forall s. StanzaC s => Lens s a) -> Lens Stanza a
lift f inj (IQRequestS     s) = IQRequestS     <$> f inj s
lift f inj (IQResultS      s) = IQResultS      <$> f inj s
lift f inj (IQErrorS       s) = IQErrorS       <$> f inj s
lift f inj (MessageS       s) = MessageS       <$> f inj s
lift f inj (MessageErrorS  s) = MessageErrorS  <$> f inj s
lift f inj (PresenceS      s) = PresenceS      <$> f inj s
lift f inj (PresenceErrorS s) = PresenceErrorS <$> f inj s

instance StanzaC Stanza where
    from = lift from
    to   = lift to
    lang = lift lang

class HasStanzaID s where
    sid :: Lens s StanzaID

instance HasStanzaID IQRequest where
    sid inj m@(IQRequest {iqRequestID = i}) = (\i' -> m{iqRequestID = i'}) <$>
                                                  inj i

instance HasStanzaID IQResult where
    sid inj m@(IQResult {iqResultID = i}) = (\i' -> m{iqResultID = i'}) <$>
                                                  inj i

instance HasStanzaID IQError where
    sid inj m@(IQError {iqErrorID = i}) = (\i' -> m{iqErrorID = i'}) <$>
                                                  inj i
class MaybeHasStanzaID s where
    msid :: Lens s (Maybe StanzaID)

instance MaybeHasStanzaID Message where
    msid inj m@(Message {messageID = i}) = (\i' -> m{messageID = i'}) <$>
                                                  inj i

($.) :: Lens a b -> a -> b
f $. x = getConst $ f Const x
