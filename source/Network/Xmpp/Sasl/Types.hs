{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Sasl.Types where

import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.ByteString(ByteString)
import qualified Data.Text as Text
import           Network.Xmpp.Types
import           Control.Concurrent.STM

-- | Signals a (non-fatal) SASL authentication error condition.
data AuthFailure = -- | No mechanism offered by the server was matched
                   -- by the provided acceptable mechanisms; wraps the
                   -- mechanisms offered by the server
                   AuthNoAcceptableMechanism [Text.Text]
                 | AuthStreamFailure XmppFailure -- TODO: Remove
                   -- | A SASL failure element was encountered
                 | AuthSaslFailure SaslFailure
                   -- | The credentials provided did not conform to
                   -- the SASLprep Stringprep profile
                 | AuthIllegalCredentials
                   -- | Other failure; more information is available
                   -- in the log
                 | AuthOtherFailure
                 deriving Show

instance Error AuthFailure where
    noMsg = AuthOtherFailure

data SaslElement = SaslSuccess   (Maybe Text.Text)
                 | SaslChallenge (Maybe Text.Text)

type Pairs = [(ByteString, ByteString)]

-- | Tuple defining the SASL Handler's name, and a SASL mechanism computation.
-- The SASL mechanism is a stateful @Stream@ computation, which has the
-- possibility of resulting in an authentication error.
type SaslHandler = (Text.Text, (TMVar Stream -> IO (Either XmppFailure (Maybe AuthFailure))))
