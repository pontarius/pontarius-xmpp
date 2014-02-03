{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Xmpp.Concurrent.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad.Error
import qualified Data.ByteString as BS
import           Data.Default
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable
import           Data.XML.Types (Element)
import           Network
import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Types

type StanzaHandler =  (Stanza -> IO (Either XmppFailure ()) ) -- ^ outgoing stanza
                   -> Stanza       -- ^ stanza to handle
                   -> [Annotation] -- ^ annotations added by previous handlers
                   -> IO [(Stanza, [Annotation])]  -- ^ modified stanzas and
                                                   -- /additional/ annotations

type Resource = Text

-- | SASL handlers and the desired JID resource
--
-- Nothing to disable authentication
--
-- The allowed SASL mecahnism can depend on the connection state. For example,
-- 'plain' should be avoided unless the connection state is 'Secured'
--
-- It is recommended to leave the resource up to the server
type AuthData = Maybe (ConnectionState -> [SaslHandler] , Maybe Resource)

data Annotation = forall f.(Typeable f, Show f) => Annotation{fromAnnotation :: f}

instance Show Annotation where
    show (Annotation x) = "Annotation{ fromAnnotation = " ++ show x ++ "}"

type Annotated a = (a, [Annotation])

-- | Retrieve the first matching annotation
getAnnotation :: Typeable b => Annotated a -> Maybe b
getAnnotation = foldr (\(Annotation a) b -> maybe b Just $ cast a) Nothing . snd

data Plugin' = Plugin' { inHandler :: Stanza
                                      -> [Annotation]
                                      -> IO [(Stanza, [Annotation])]
                       , outHandler :: Stanza -> IO (Either XmppFailure ())
                          -- | In order to allow plugins to tie the knot (Plugin
                          -- / Session) we pass the plugin the completed Session
                          -- once it exists
                       , onSessionUp :: Session -> IO ()
                       }

type Plugin = (Stanza -> IO (Either XmppFailure ()))
              -> ErrorT XmppFailure IO Plugin'

-- | Configuration for the @Session@ object.
data SessionConfiguration = SessionConfiguration
    { -- | Configuration for the @Stream@ object.
      sessionStreamConfiguration :: StreamConfiguration
      -- | Handler to be run when the conection to the XMPP server is
      -- closed. See also 'reconnect' and 'reconnect\'' for easy
      -- reconnection. The default does nothing
    , onConnectionClosed         :: Session -> XmppFailure -> IO ()
      -- | Function to generate new stanza identifiers.
    , sessionStanzaIDs           :: IO (IO Text)
      -- | Plugins can modify incoming and outgoing stanzas, for example to en-
      -- and decrypt them, respectively
    , plugins                    :: [Plugin]
      -- | Enable roster handling according to rfc 6121. See 'getRoster' to
      -- acquire the current roster
    , enableRoster               :: Bool
    }

instance Default SessionConfiguration where
    def = SessionConfiguration { sessionStreamConfiguration = def
                               , onConnectionClosed = \_ _ -> return ()
                               , sessionStanzaIDs = do
                                     idRef <- newTVarIO 1
                                     return . atomically $ do
                                         curId <- readTVar idRef
                                         writeTVar idRef (curId + 1 :: Integer)
                                         return . Text.pack . show $ curId
                               , plugins = []
                               , enableRoster = True
                               }

-- | Handlers to be run when the Xmpp session ends and when the Xmpp connection is
-- closed.
data EventHandlers = EventHandlers
    { connectionClosedHandler :: XmppFailure -> IO ()
    }

-- | Interrupt is used to signal to the reader thread that it should stop. Th contained semphore signals the reader to resume it's work.
data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"

instance Ex.Exception Interrupt

type WriteSemaphore = TMVar (BS.ByteString -> IO (Either XmppFailure ()))

-- | The Session object represents a single session with an XMPP server. You can
-- use 'session' to establish a session
data Session = Session
    { stanzaCh :: TChan (Stanza, [Annotation]) -- All stanzas
    , iqHandlers :: TVar IQHandlers
      -- Writing lock, so that only one thread could write to the stream at any
      -- given time.
      -- Fields below are from Context.
    , writeSemaphore :: WriteSemaphore
    , readerThread :: ThreadId
    , idGenerator :: IO Text
      -- | Lock (used by withStream) to make sure that a maximum of one
      -- Stream action is executed at any given time.
    , streamRef :: TMVar Stream
    , eventHandlers :: TMVar EventHandlers
    , stopThreads :: IO ()
    , rosterRef :: TVar Roster
    , conf :: SessionConfiguration
    , sendStanza' :: Stanza -> IO (Either XmppFailure ())
    , sRealm :: HostName
    , sSaslCredentials :: Maybe (ConnectionState -> [SaslHandler] , Maybe Text)
    , reconnectWait :: TVar Int
    }

-- | IQHandlers holds the registered channels for incoming IQ requests and
-- TMVars of and TMVars for expected IQ responses (the second Text represent a
-- stanza identifier.
type IQHandlers = ( Map.Map (IQRequestType, Text) (TChan IQRequestTicket)
                  , Map.Map Text (Either (Maybe Jid) Jid,
                                  TMVar (Maybe (Annotated IQResponse)))
                  )

-- | Contains whether or not a reply has been sent, and the IQ request body to
-- reply to.
data IQRequestTicket = IQRequestTicket
    { answerTicket :: Either StanzaError (Maybe Element)
                      -> IO (Maybe (Either XmppFailure ()))
                      -- ^ Return Nothing when the IQ request was already
                      -- answered before, Just (Right ()) when it was
                      -- sucessfully answered and Just (Left error) when the
                      -- answer was attempted, but failed (e.g. there is a
                      -- connection failure)
    , iqRequestBody :: IQRequest
      -- | Annotations set by plugins in receive
    , iqRequestAnnotations :: [Annotation]
    }

-- | Error that can occur during sendIQ'
data IQSendError = IQSendError XmppFailure -- There was an error sending the IQ stanza
                 | IQTimeOut -- No answer was received during the allotted time
                   deriving (Show, Eq)
