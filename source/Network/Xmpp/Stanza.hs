{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Stanza related functions and constants
--

module Network.Xmpp.Stanza where

import Data.XML.Types
import Network.Xmpp.Types
import Network.Xmpp.Lens

-- | Request subscription with an entity.
presenceSubscribe :: Jid -> Presence
presenceSubscribe to' = presence { presenceTo = Just to'
                                 , presenceType = Subscribe
                                 }

-- | Approve a subscripton of an entity.
presenceSubscribed :: Jid -> Presence
presenceSubscribed to' = presence { presenceTo = Just to'
                                  , presenceType = Subscribed
                                  }

-- | End a subscription with an entity.
presenceUnsubscribe :: Jid -> Presence
presenceUnsubscribe to' = presence { presenceTo = Just to'
                                   , presenceType = Unsubscribed
                                   }

-- | Signal to the server that the client is available for communication.
presenceOnline :: Presence
presenceOnline = presence

-- | Signal to the server that the client is no longer available for
-- communication.
presenceOffline :: Presence
presenceOffline = presence {presenceType = Unavailable}

-- | Produce an answer message with the given payload, setting "from" to the
-- "to" attributes in the original message. Produces a 'Nothing' value of the
-- provided message message has no "from" attribute. Sets the "from" attribute
-- to 'Nothing' to let the server assign one.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload' =
    Just Message{ messageFrom    = Nothing
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload'
                , ..
                }
answerMessage _ _ = Nothing

-- | Add a recipient to a presence notification.
presTo :: Presence -> Jid -> Presence
presTo pres to' = pres{presenceTo = Just to'}

-- | Create a StanzaError with @condition@ and the 'associatedErrorType'. Leave
-- the error text and the application specific condition empty
mkStanzaError :: StanzaErrorCondition -- ^ condition
              -> StanzaError
mkStanzaError condition = StanzaError (associatedErrorType condition)
                                      condition Nothing Nothing

-- | Create an IQ error response to an IQ request using the given condition. The
-- error type is derived from the condition using 'associatedErrorType' and
-- both text and the application specific condition are left empty
iqError :: StanzaErrorCondition -> IQRequest -> IQError
iqError condition (IQRequest iqid from' _to lang' _tp _bd) =
    IQError iqid Nothing from' lang' (mkStanzaError condition) Nothing


-- | Create an IQ Result matching an IQ request
iqResult ::  Maybe Element -> IQRequest -> IQResult
iqResult pl iqr = IQResult
              { iqResultID   = iqRequestID iqr
              , iqResultFrom = Nothing
              , iqResultTo   = view from iqr
              , iqResultLangTag = view lang iqr
              , iqResultPayload = pl
              }

-- | The RECOMMENDED error type associated with an error condition. The
-- following conditions allow for multiple types
--
-- * 'FeatureNotImplemented': 'Cancel' or 'Modify' (returns 'Cancel')
--
-- * 'PolicyViolation': 'Modify' or 'Wait' ('Modify')
--
-- * 'RemoteServerTimeout': 'Wait' or unspecified other ('Wait')
--
-- * 'UndefinedCondition': Any condition ('Cancel')
associatedErrorType :: StanzaErrorCondition -> StanzaErrorType
associatedErrorType BadRequest            = Modify
associatedErrorType Conflict              = Cancel
associatedErrorType FeatureNotImplemented = Cancel -- Or Modify
associatedErrorType Forbidden             = Auth
associatedErrorType Gone{}                = Cancel
associatedErrorType InternalServerError   = Cancel
associatedErrorType ItemNotFound          = Cancel
associatedErrorType JidMalformed          = Modify
associatedErrorType NotAcceptable         = Modify
associatedErrorType NotAllowed            = Cancel
associatedErrorType NotAuthorized         = Auth
associatedErrorType PolicyViolation       = Modify -- Or Wait
associatedErrorType RecipientUnavailable  = Wait
associatedErrorType Redirect{}            = Modify
associatedErrorType RegistrationRequired  = Auth
associatedErrorType RemoteServerNotFound  = Cancel
associatedErrorType RemoteServerTimeout   = Wait -- Possibly Others
associatedErrorType ResourceConstraint    = Wait
associatedErrorType ServiceUnavailable    = Cancel
associatedErrorType SubscriptionRequired  = Auth
associatedErrorType UndefinedCondition    = Cancel -- This can be anything
associatedErrorType UnexpectedRequest     = Modify
