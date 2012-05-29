module Network.Xmpp.Sasl.Types where

import           Control.Monad.Error
import           Data.Text
import           Network.Xmpp.Types

data AuthError = AuthXmlError
               | AuthMechanismError [Text] -- ^ Wraps mechanisms offered
               | AuthChallengeError
               | AuthStreamError StreamError -- ^ Stream error on stream restart
               | AuthConnectionError -- ^ No host name set in state
               | AuthError -- General instance used for the Error instance
                 deriving Show

instance Error AuthError where
    noMsg = AuthError