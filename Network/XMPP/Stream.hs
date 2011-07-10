-----------------------------------------------------------------------------
--
-- Module      :  Network.XMPP.Stream
-- Copyright   :  Copyright © 2011, Jon Kristensen
-- License     :  UnknownLicense "LGPL3"
--
-- Maintainer  :  jon.kristensen@pontarius.org
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.XMPP.Stream (
isTLSSecured,
xmlEnumerator,
xmlReader,
presenceToXML,
iqToXML,
messageToXML,
parsePresence,
parseIQ,
parseMessage
) where

import Network.XMPP.Address hiding (fromString)
import qualified Network.XMPP.Address as X
import Network.XMPP.Types
import Network.XMPP.Utilities
import Network.XMPP.TLS
import Network.XMPP.Stanza
import qualified Control.Exception as CE
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)
import Network.TLS
import Network.TLS.Cipher
import Data.Enumerator (($$), Iteratee, continue, joinI,
                        run, run_, yield)
import Data.Enumerator.Binary (enumHandle, enumFile)
import Text.XML.Enumerator.Parse (parseBytes, decodeEntities)
import Text.XML.Enumerator.Document (fromEvents)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL (ByteString, append, pack, fromChunks, toChunks, null)
import qualified Data.ByteString.Lazy.Char8 as DBLC (append, pack, unpack)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import Data.Maybe

import Data.XML.Types

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.String (IsString(..))

isTLSSecured :: TLSState -> Bool
isTLSSecured (PostHandshake _) = True
isTLSSecured _ = False


-- Reads from the provided handle or TLS context and sends the events to the
-- internal event channel.

xmlEnumerator :: Chan (InternalEvent s m) -> Either Handle TLSCtx -> IO ()
xmlEnumerator c s = do
  enumeratorResult <- case s of
    Left handle -> run $ enumHandle 1 handle $$ joinI $
                   parseBytes decodeEntities $$ xmlReader c
    Right tlsCtx -> run $ enumTLS tlsCtx $$ joinI $
                    parseBytes decodeEntities $$ xmlReader c
  case enumeratorResult of
    Right _ ->
      writeChan c $ IEE EnumeratorDone
    Left e ->
      writeChan c $ IEE (EnumeratorException e)
  where
    -- Behaves like enumHandle, but reads from the TLS context instead
    enumTLS :: TLSCtx -> E.Enumerator DB.ByteString IO b
    enumTLS c s = loop c s

    loop :: TLSCtx -> E.Step DB.ByteString IO b -> E.Iteratee DB.ByteString IO b
    loop c (E.Continue k) = do
      d <- recvData c
      case DBL.null d of
        True  -> loop c (E.Continue k)
        False -> k (E.Chunks $ DBL.toChunks d) E.>>== loop c
    loop _ step = E.returnI step


xmlReader :: Chan (InternalEvent s m) -> Iteratee Event IO (Maybe Event)

xmlReader c = xmlReader_ c [] 0


xmlReader_ :: Chan (InternalEvent s m) -> [Event] -> Int ->
             Iteratee Event IO (Maybe Event)

xmlReader_ ch [EventBeginDocument] 0 = xmlReader_ ch [] 0

-- TODO: Safe to start change level here? We are doing this since the stream can
-- restart.
-- TODO: l < 2?
xmlReader_ ch [EventBeginElement name attribs] l
  | l < 3 && nameLocalName name == DT.pack "stream" &&
    namePrefix name == Just (DT.pack "stream") = do
      liftIO $ writeChan ch $ IEE $ EnumeratorXML $ XEBeginStream $ "StreamTODO"
      xmlReader_ ch [] 1

xmlReader_ ch [EventEndElement name] 1
  | namePrefix name == Just (DT.pack "stream") &&
    nameLocalName name == DT.pack "stream" = do
      liftIO $ writeChan ch $ IEE $ EnumeratorXML $ XEEndStream
      return Nothing

-- Check if counter is one to forward it to related function.
-- Should replace "reverse ((EventEndElement n):es)" with es
-- ...
xmlReader_ ch ((EventEndElement n):es) 1
  | nameLocalName n == DT.pack "proceed" = do
    liftIO $ writeChan ch $ IEE $ EnumeratorXML $ XEProceed
    E.yield Nothing (E.Chunks [])
  | otherwise = do
    -- liftIO $ putStrLn "Got an IEX Event..."
    liftIO $ writeChan ch $ IEE $ EnumeratorXML $ (processEventList (DL.reverse ((EventEndElement n):es)))
    xmlReader_ ch [] 1

-- Normal condition, buffer the event to events list.
xmlReader_ ch es co = do
  head <- EL.head
  let co' = counter co head
  -- liftIO $ putStrLn $ show co' ++ "\t" ++ show head    -- for test
  case head of
    Just e -> xmlReader_ ch (e:es) co'
    Nothing -> xmlReader_ ch es co'


-- TODO: Generate real event.
processEventList :: [Event] -> XMLEvent
processEventList e
  | namePrefix name == Just (DT.pack "stream") &&
    nameLocalName name == DT.pack "features" = XEFeatures "FeaturesTODO"
  | nameLocalName name == DT.pack "challenge" =
    let EventContent (ContentText c) = head es in XEChallenge $ Chal $ DT.unpack c
  | nameLocalName name == DT.pack "success" =
    let EventContent (ContentText c) = head es in XESuccess $ Succ $ "" -- DT.unpack c
  | nameLocalName name == DT.pack "iq" = XEIQ $ parseIQ $ eventsToElement e
  | nameLocalName name == DT.pack "presence" = XEPresence $ parsePresence $ eventsToElement e
  | nameLocalName name == DT.pack "message" = XEMessage $ parseMessage $ eventsToElement e
  | otherwise = XEOther $ elementToString $ Just (eventsToElement e)
      where
        (EventBeginElement name attribs) = head e
        es = tail e

eventsToElement :: [Event] -> Element
eventsToElement e = do
  documentRoot $ fromJust (run_ $ enum e $$ fromEvents)
    where
      enum :: [Event] -> E.Enumerator Event Maybe Document
      enum e_ (E.Continue k) = k $ E.Chunks e_
      enum e_ step = E.returnI step

counter :: Int -> Maybe Event -> Int
counter c (Just (EventBeginElement _ _)) = (c + 1)
counter c (Just (EventEndElement _) )    = (c - 1)
counter c _                       = c

presenceToXML :: Presence -> String
presenceToXML p = "<presence" ++ from ++ id' ++ to ++ type' ++ ">" ++
                  (elementsToString $ presencePayload p) ++ "</presence>"
  where
    from :: String
    from = case presenceFrom p of
      -- TODO: Lower-case
      Just s -> " from='" ++ (show s) ++ "'"
      Nothing -> ""

    id' :: String
    id' = case presenceID p of
      Just (SID s) -> " id='" ++ s ++ "'"
      Nothing -> ""

    to :: String
    to = case presenceTo p of
      -- TODO: Lower-case
      Just s -> " to='" ++ (show s) ++ "'"
      Nothing -> ""

    type' :: String
    type' = case presenceType p of
      Available -> ""
      t -> " type='" ++ (presenceTypeToString t) ++ "'"

iqToXML :: IQ -> String
iqToXML (IQReq (IQGet { iqRequestID = i, iqRequestPayload = p, iqRequestFrom = f, iqRequestTo = t })) =
  let type' = " type='get'" in "<iq" ++ from ++ id' ++ to ++ type' ++ ">" ++ (elementToString (Just p)) ++ "</iq>"
  where
    from :: String
    from = case f of
      -- TODO: Lower-case
      Just s -> " from='" ++ (show s) ++ "'"
      Nothing -> ""

    id' :: String
    id' = case i of
      Just (SID s) -> " id='" ++ s ++ "'"
      Nothing -> ""

    to :: String
    to = case t of
      -- TODO: Lower-case
      Just s -> " to='" ++ (show s) ++ "'"
      Nothing -> ""

iqToXML (IQReq (IQSet { iqRequestID = i, iqRequestPayload = p, iqRequestFrom = f, iqRequestTo = t })) =
  let type' = " type='set'" in "<iq" ++ from ++ id' ++ to ++ type' ++ ">" ++ (elementToString (Just p)) ++ "</iq>"
  where
    from :: String
    from = case f of
      -- TODO: Lower-case
      Just s -> " from='" ++ (show s) ++ "'"
      Nothing -> ""

    id' :: String
    id' = case i of
      Just (SID s) -> " id='" ++ s ++ "'"
      Nothing -> ""

    to :: String
    to = case t of
      -- TODO: Lower-case
      Just s -> " to='" ++ (show s) ++ "'"
      Nothing -> ""

iqToXML (IQRes (IQResult { iqResponseID = i, iqResponsePayload = p, iqResponseFrom = f, iqResponseTo = t })) =
  let type' = " type='result'" in "<iq" ++ from ++ id' ++ to ++ type' ++ ">" ++ (elementToString p) ++ "</iq>"
  where
    from :: String
    from = case f of
      -- TODO: Lower-case
      Just s -> " from='" ++ (show s) ++ "'"
      Nothing -> ""

    id' :: String
    id' = case i of
      Just (SID s) -> " id='" ++ s ++ "'"
      Nothing -> ""

    to :: String
    to = case t of
      -- TODO: Lower-case
      Just s -> " to='" ++ (show s) ++ "'"
      Nothing -> ""

-- TODO: Turn message errors into XML.

messageToXML :: Message -> String
messageToXML Message { messageID = i, messageFrom = f, messageTo = t, messagePayload = p, messageType = ty } = "<message" ++ from ++ id' ++ to ++ type' ++ ">" ++
                  (elementsToString $ p) ++ "</message>"
  where
    from :: String
    from = case f of
      -- TODO: Lower-case
      Just s -> " from='" ++ (show s) ++ "'"
      Nothing -> ""

    id' :: String
    id' = case i of
      Just (SID s) -> " id='" ++ s ++ "'"
      Nothing -> ""

    to :: String
    to = case t of
      -- TODO: Lower-case
      Just s -> " to='" ++ (show s) ++ "'"
      Nothing -> ""

    type' :: String
    type' = case ty of
      Normal -> ""
      t -> " type='" ++ (messageTypeToString t) ++ "'"


parseIQ :: Element -> IQ
parseIQ e | typeAttr == "get" = let (Just payloadMust) = payload
                                in IQReq (IQGet idAttr fromAttr toAttr Nothing
                                   payloadMust)
          | typeAttr == "set" = let (Just payloadMust) = payload
                                in IQReq (IQSet idAttr fromAttr toAttr Nothing
                                   payloadMust)
          | typeAttr == "result" = IQRes (IQResult idAttr fromAttr toAttr
                                   Nothing payload)

  where
    -- TODO: Many duplicate functions from parsePresence.

    payload :: Maybe Element
    payload = case null (elementChildren e) of
      True -> Nothing
      False -> Just $ head $ elementChildren e

    typeAttr :: String
    typeAttr = case attributeText typeName e of
      -- Nothing -> Nothing
      Just a -> DT.unpack a

    fromAttr :: Maybe Address
    fromAttr = case attributeText fromName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    toAttr :: Maybe Address
    toAttr = case attributeText toName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    idAttr :: Maybe StanzaID
    idAttr = case attributeText idName e of
      Nothing -> Nothing
      Just a -> Just (SID (DT.unpack a))

    typeName :: Name
    typeName = fromString "type"

    fromName :: Name
    fromName = fromString "from"

    toName :: Name
    toName = fromString "to"

    idName :: Name
    idName = fromString "id"

-- TODO: Parse xml:lang

parsePresence :: Element -> Presence
parsePresence e = Presence idAttr fromAttr toAttr Nothing typeAttr (elementChildren e)
  where
    -- TODO: Many duplicate functions from parseIQ.

    typeAttr :: PresenceType
    typeAttr = case attributeText typeName e of
      Just t -> stringToPresenceType $ DT.unpack t
      Nothing -> Available

    fromAttr :: Maybe Address
    fromAttr = case attributeText fromName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    toAttr :: Maybe Address
    toAttr = case attributeText toName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    idAttr :: Maybe StanzaID
    idAttr = case attributeText idName e of
      Nothing -> Nothing
      Just a -> Just (SID (DT.unpack a))

    fromName :: Name
    fromName = fromString "from"

    typeName :: Name
    typeName = fromString "type"

    toName :: Name
    toName = fromString "to"

    idName :: Name
    idName = fromString "id"

parseMessage :: Element -> Message
parseMessage e = Message idAttr fromAttr toAttr Nothing typeAttr (elementChildren e)
  where
    -- TODO: Many duplicate functions from parseIQ.

    typeAttr :: MessageType
    typeAttr = case attributeText typeName e of
      Just t -> stringToMessageType $ DT.unpack t
      Nothing -> Normal

    fromAttr :: Maybe Address
    fromAttr = case attributeText fromName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    toAttr :: Maybe Address
    toAttr = case attributeText toName e of
      Nothing -> Nothing
      Just a -> X.fromString $ DT.unpack a

    idAttr :: Maybe StanzaID
    idAttr = case attributeText idName e of
      Nothing -> Nothing
      Just a -> Just (SID (DT.unpack a))

    fromName :: Name
    fromName = fromString "from"

    typeName :: Name
    typeName = fromString "type"

    toName :: Name
    toName = fromString "to"

    idName :: Name
    idName = fromString "id"

-- stringToPresenceType "available" = Available
-- stringToPresenceType "away" = Away
-- stringToPresenceType "chat" = Chat
-- stringToPresenceType "dnd" = DoNotDisturb
-- stringToPresenceType "xa" = ExtendedAway

stringToPresenceType "available" = Available -- TODO: Some client sent this

stringToPresenceType "probe" = Probe
-- stringToPresenceType "error" = PresenceError -- TODO: Special case

stringToPresenceType "unavailable" = Unavailable
stringToPresenceType "subscribe" = Subscribe
stringToPresenceType "subscribed" = Subscribed
stringToPresenceType "unsubscribe" = Unsubscribe
stringToPresenceType "unsubscribed" = Unsubscribed

-- presenceTypeToString Available = "available"

-- presenceTypeToString Away = "away"
-- presenceTypeToString Chat = "chat"
-- presenceTypeToString DoNotDisturb = "dnd"
-- presenceTypeToString ExtendedAway = "xa"

presenceTypeToString Unavailable = "unavailable"

presenceTypeToString Probe = "probe"
-- presenceTypeToString PresenceError = "error" -- TODO: Special case

presenceTypeToString Subscribe = "subscribe"
presenceTypeToString Subscribed = "subscribed"
presenceTypeToString Unsubscribe = "unsubscribe"
presenceTypeToString Unsubscribed = "unsubscribed"

stringToMessageType "chat" = Chat
stringToMessageType "error" = Error
stringToMessageType "groupchat" = Groupchat
stringToMessageType "headline" = Headline
stringToMessageType "normal" = Normal
stringToMessageType s = OtherMessageType s

messageTypeToString Chat = "chat"
messageTypeToString Error = "error"
messageTypeToString Groupchat = "groupchat"
messageTypeToString Headline = "headline"
messageTypeToString Normal = "normal"
messageTypeToString (OtherMessageType s) = s
