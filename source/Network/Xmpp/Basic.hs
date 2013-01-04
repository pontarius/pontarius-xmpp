module Network.Xmpp.Basic
  ( Connection(..)
  , ConnectionState(..)
  , connectTcp
  , newSession
  , withConnection
  , startTls
  , simpleAuth
  , auth
  , scramSha1
  , digestMd5
  , plain
  , closeConnection
  , pushStanza
  , pullStanza
  , closeConnection
  , endContext
  , setConnectionClosedHandler
 )

       where

import Network.Xmpp.Connection
import Network.Xmpp.Sasl
import Network.Xmpp.Session
import Network.Xmpp.Stream
import Network.Xmpp.Tls
import Network.Xmpp.Types
import Network.Xmpp.Concurrent