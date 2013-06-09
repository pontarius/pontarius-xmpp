{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Sasl.Types where

import           Control.Monad.State.Strict
import           Data.ByteString(ByteString)
import qualified Data.Text as Text
import           Network.Xmpp.Types

data SaslElement = SaslSuccess   (Maybe Text.Text)
                 | SaslChallenge (Maybe Text.Text)

type Pairs = [(ByteString, ByteString)]

-- | Tuple defining the SASL Handler's name, and a SASL mechanism computation.
-- The SASL mechanism is a stateful @Stream@ computation, which has the
-- possibility of resulting in an authentication error.
type SaslHandler = (Text.Text, StateT StreamState IO (Either XmppFailure (Maybe AuthFailure)))
