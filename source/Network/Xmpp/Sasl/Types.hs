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
                                     -- itself
               | AuthStreamError StreamError -- ^ Stream error on stream restart
               -- TODO: Rename AuthConnectionError?
               | AuthConnectionError -- ^ Connection is closed
               | AuthError -- General instance used for the Error instance
               | AuthSaslFailure SaslFailure -- ^ Defined SASL error condition
               | AuthStringPrepError -- ^ StringPrep failed
                 deriving Show

instance Error AuthError where
    noMsg = AuthError

data SaslElement = SaslSuccess   (Maybe Text.Text)
                 | SaslChallenge (Maybe Text.Text)

-- | SASL mechanism XmppConnection computation, with the possibility of throwing
-- an authentication error.
type SaslM a = ErrorT AuthError (StateT XmppConnection IO) a

type Pairs = [(ByteString, ByteString)]

-- | Tuple defining the SASL Handler's name, and a SASL mechanism computation
type SaslHandler = (Text.Text, SaslM ())
