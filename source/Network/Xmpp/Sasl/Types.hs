{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Sasl.Types where

import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.ByteString(ByteString)
import qualified Data.Text as Text
import           Network.Xmpp.Types

data AuthFailure = AuthXmlFailure
               | AuthNoAcceptableMechanism [Text.Text] -- ^ Wraps mechanisms
                                                       -- offered
               | AuthChallengeFailure
               | AuthServerAuthFailure -- ^ The server failed to authenticate
                                     -- itself
               | AuthStreamFailure XmppFailure -- ^ Stream error on stream restart
               -- TODO: Rename AuthConnectionFailure?
               | AuthNoStream
               | AuthFailure -- General instance used for the Error instance
               | AuthSaslFailure SaslFailure -- ^ Defined SASL error condition
               | AuthStringPrepFailure -- ^ StringPrep failed
                 deriving Show

instance Error AuthFailure where
    noMsg = AuthFailure

data SaslElement = SaslSuccess   (Maybe Text.Text)
                 | SaslChallenge (Maybe Text.Text)

type Pairs = [(ByteString, ByteString)]

-- | Tuple defining the SASL Handler's name, and a SASL mechanism computation.
-- The SASL mechanism is a stateful @Stream@ computation, which has the
-- possibility of resulting in an authentication error.
type SaslHandler = (Text.Text, ErrorT AuthFailure (StateT Stream IO) ())
