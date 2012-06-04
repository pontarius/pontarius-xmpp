module Network.Xmpp.Sasl.Types where

import Control.Monad.Error
import Control.Monad.State.Strict
import Data.Text
import Network.Xmpp.Types
import Data.ByteString(ByteString)

data AuthError = AuthXmlError
               | AuthMechanismError [Text] -- ^ Wraps mechanisms offered
               | AuthChallengeError
               | AuthStreamError StreamError -- ^ Stream error on stream restart
               | AuthConnectionError -- ^ No host name set in state
               | AuthError -- General instance used for the Error instance
               | AuthSaslFailure SaslFailure -- ^ defined SASL error condition
                 deriving Show

instance Error AuthError where
    noMsg = AuthError

type SaslM a = ErrorT AuthError (StateT XmppConnection IO) a

type Pairs = [(ByteString, ByteString)]