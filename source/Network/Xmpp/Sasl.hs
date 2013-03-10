{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

-- Submodule for functionality related to SASL negotation:
-- authentication functions, SASL functionality, bind functionality,
-- and the legacy `{urn:ietf:params:xml:ns:xmpp-session}session'
-- functionality.

module Network.Xmpp.Sasl
    ( xmppSasl
    , digestMd5
    , scramSha1
    , plain
    , auth
    ) where

import           Control.Applicative
import           Control.Arrow (left)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Maybe (fromJust, isJust)

import qualified Crypto.Classes as CC

import qualified Data.Binary as Binary
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.List as L
import           Data.Word (Word8)

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Network.Xmpp.Stream
import           Network.Xmpp.Types

import           System.Log.Logger (debugM, errorM)
import qualified System.Random as Random

import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Sasl.Mechanisms

import           Control.Concurrent.STM.TMVar

import           Control.Exception

import           Data.XML.Pickle
import           Data.XML.Types

import           Network.Xmpp.Types
import           Network.Xmpp.Marshal

import           Control.Monad.State(modify)

import           Control.Concurrent.STM.TMVar

import           Control.Monad.Error

-- | Uses the first supported mechanism to authenticate, if any. Updates the
-- state with non-password credentials and restarts the stream upon
-- success. Returns `Nothing' on success, an `AuthFailure' if
-- authentication fails, or an `XmppFailure' if anything else fails.
xmppSasl :: [SaslHandler] -- ^ Acceptable authentication mechanisms and their
                       -- corresponding handlers
         -> TMVar Stream
         -> IO (Either XmppFailure (Maybe AuthFailure))
xmppSasl handlers stream = do
    debugM "Pontarius.Xmpp" "xmppSasl: Attempts to authenticate..."
    flip withStream stream $ do
        -- Chooses the first mechanism that is acceptable by both the client and the
        -- server.
        mechanisms <- gets $ streamSaslMechanisms . streamFeatures
        case (filter (\(name, _) -> name `elem` mechanisms)) handlers of
            [] -> return $ Right $ Just $ AuthNoAcceptableMechanism mechanisms
            (_name, handler):_ -> do
                cs <- gets streamState
                case cs of
                    Closed -> do
                        lift $ errorM "Pontarius.Xmpp" "xmppSasl: Stream state closed."
                        return . Left $ XmppNoStream
                    _ -> runErrorT $ do
                           -- TODO: Log details about handler? SaslHandler "show" instance?
                           lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Performing handler..."
                           r <- ErrorT handler
                           case r of
                               Just ae -> do
                                   lift $ lift $ errorM "Pontarius.Xmpp" $
                                       "xmppSasl: AuthFailure encountered: " ++
                                           show ae
                                   return $ Just ae
                               Nothing -> do
                                   lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Authentication successful, restarting stream."
                                   _ <- ErrorT restartStream
                                   lift $ lift $ debugM "Pontarius.Xmpp" "xmppSasl: Stream restarted."
                                   return Nothing

-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> TMVar Stream
     -> IO (Either XmppFailure (Maybe AuthFailure))
auth mechanisms resource con = runErrorT $ do
    ErrorT $ xmppSasl mechanisms con
    jid <- ErrorT $ xmppBind resource con
    ErrorT $ flip withStream con $ do
        s <- get
        case establishSession $ streamConfiguration s of
            False -> return $ Right Nothing
            True -> do
                _ <- lift $ startSession con
                return $ Right Nothing
    return Nothing

-- Produces a `bind' element, optionally wrapping a resource.
bindBody :: Maybe Text -> Element
bindBody = pickleElem $
               -- Pickler to produce a
               -- "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>"
               -- element, with a possible "<resource>[JID]</resource>"
               -- child.
               xpBind . xpOption $ xpElemNodes "resource" (xpContent xpId)

-- Sends a (synchronous) IQ set request for a (`Just') given or server-generated
-- resource and extract the JID from the non-error response.
xmppBind  :: Maybe Text -> TMVar Stream -> IO (Either XmppFailure Jid)
xmppBind rsrc c = runErrorT $ do
    lift $ debugM "Pontarius.Xmpp" "Attempts to bind..."
    answer <- ErrorT $ pushIQ "bind" Nothing Set Nothing (bindBody rsrc) c
    case answer of
        Right IQResult{iqResultPayload = Just b} -> do
            lift $ debugM "Pontarius.XMPP" "xmppBind: IQ result received; unpickling JID..."
            let jid = unpickleElem xpJid b
            case jid of
                Right jid' -> do
                    lift $ debugM "Pontarius.XMPP" $ "xmppBind: JID unpickled: " ++ show jid'
                    ErrorT $ withStream (do
                                      modify $ \s -> s{streamJid = Just jid'}
                                      return $ Right jid') c -- not pretty
                    return jid'
                otherwise -> do
                    lift $ errorM "Pontarius.XMPP" $ "xmppBind: JID could not be unpickled from: "
                        ++ show b
                    throwError $ XmppOtherFailure $ "xmppBind: JID could not be unpickled from: " ++ show b
        otherwise -> do
            lift $ errorM "Pontarius.XMPP" "xmppBind: IQ error received."
            throwError $ XmppOtherFailure "bind: failed to bind"
  where
    -- Extracts the character data in the `jid' element.
    xpJid :: PU [Node] Jid
    xpJid = xpBind $ xpElemNodes jidName (xpContent xpPrim)
    jidName = "{urn:ietf:params:xml:ns:xmpp-bind}jid"

-- A `bind' element pickler.
xpBind  :: PU [Node] b -> PU [Node] b
xpBind c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c

sessionXml :: Element
sessionXml = pickleElem
    (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session")
    ()

sessionIQ :: Stanza
sessionIQ = IQRequestS $ IQRequest { iqRequestID      = "sess"
                                   , iqRequestFrom    = Nothing
                                   , iqRequestTo      = Nothing
                                   , iqRequestLangTag = Nothing
                                   , iqRequestType    = Set
                                   , iqRequestPayload = sessionXml
                                   }

-- Sends the session IQ set element and waits for an answer. Throws an error if
-- if an IQ error stanza is returned from the server.
startSession :: TMVar Stream -> IO Bool
startSession con = do
    debugM "Pontarius.XMPP" "startSession: Pushing `session' IQ set stanza..."
    answer <- pushIQ "session" Nothing Set Nothing sessionXml con
    case answer of
        Left e -> do
            errorM "Pontarius.XMPP" $ "startSession: Error stanza received (" ++ (show e) ++ ")"
            return False
        Right _ -> do
            debugM "Pontarius.XMPP" "startSession: Result stanza received."
            return True
