module Network.Xmpp.Sasl.Types where

import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.ByteString(ByteString)
import qualified Data.Text as Text
import           Network.Xmpp.Types

data AuthError = AuthXmlError
               | AuthNoAcceptableMechanism [Text.Text] -- ^ Wraps mechanisms
                                                       -- offered
               | AuthChallengeError
               | AuthServerAuthError -- ^ The server failed to authenticate
                                     -- himself
               | AuthStreamError StreamError -- ^ Stream error on stream restart
               | AuthConnectionError -- ^ No host name set in state
               | AuthError -- General instance used for the Error instance
               | AuthSaslFailure SaslFailure -- ^ defined SASL error condition
               | AuthStringPrepError -- ^ StringPrep failed
                 deriving Show

instance Error AuthError where
    noMsg = AuthError

type SaslM a = ErrorT AuthError (StateT XmppConnection IO) a

type Pairs = [(ByteString, ByteString)]

type SaslHandler = (Text.Text, SaslM ())