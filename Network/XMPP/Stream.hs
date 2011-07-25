-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Stream (
isTLSSecured,
xmlEnumerator,
xmlReader,
presenceToXML,
iqToXML,
messageToXML,
parsePresence,
parseIQ,
parseMessage,
langTag,
versionFromString,
versionFromNumbers
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
import Network.TLS hiding (Version)
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

import Text.Parsec (char, count, digit, eof, many, many1, oneOf, parse)
import Text.Parsec.ByteString (GenParser)

import qualified Data.ByteString.Char8 as DBC (pack)


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
  | otherwise = XEOther "TODO: Element instead of String" -- Just (eventsToElement e)
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


presenceToXML :: InternalPresence -> Element

presenceToXML (Right p) = Element "presence" attribs nodes
    where

        attribs :: [(Name, [Content])]
        attribs = stanzaNodes (presenceID p) (presenceFrom p) (presenceTo p) (presenceLangTag p) ++
                  [("type", [ContentText $ DT.pack $ show $ presenceType p])]

        nodes :: [Node]
        nodes = map (\ x -> NodeElement x) (presencePayload p)

presenceToXML (Left p) = Element "presence" attribs nodes
    where

        attribs :: [(Name, [Content])]
        attribs = stanzaNodes (presenceErrorID p) (presenceErrorFrom p) (presenceErrorTo p) (presenceErrorLangTag p) ++
                  [("type", [ContentText $ DT.pack "error"])]

        nodes :: [Node]
        nodes = case presenceErrorPayload p of
                    Just elem -> map (\ x -> NodeElement x) elem
                    Nothing -> []


iqToXML :: IQ -> Element

iqToXML = iqToXML


messageToXML :: InternalMessage -> Element

messageToXML = messageToXML


stanzaNodes :: Maybe StanzaID -> Maybe From -> Maybe To -> Maybe LangTag -> [(Name, [Content])]

stanzaNodes i f t l = if isJust $ i then [("id", [ContentText $ DT.pack $ show $ fromJust i])] else [] ++
                      if isJust $ f then [("from", [ContentText $ DT.pack $ show $ fromJust f])] else [] ++
                      if isJust $ t then [("to", [ContentText $ DT.pack $ show $ fromJust t])] else [] ++
                      if isJust $ l then [("xml:lang", [ContentText $ DT.pack $ show l])] else []


parseIQ :: Element -> IQ

parseIQ = parseIQ


parsePresence :: Element -> InternalPresence

parsePresence = parsePresence


parseMessage :: Element -> InternalMessage

parseMessage = parseMessage


stringToPresenceType :: String -> Maybe (Maybe PresenceType)

stringToPresenceType "probe" = Just $ Just Probe
stringToPresenceType "unavailable" = Just $ Just Unavailable
stringToPresenceType "subscribe" = Just $ Just Subscribe
stringToPresenceType "subscribed" = Just $ Just Subscribed
stringToPresenceType "unsubscribe" = Just $ Just Unsubscribe
stringToPresenceType "unsubscribed" = Just $ Just Unsubscribed
stringToPresenceType "error" = Just Nothing
stringToPresenceType _ = Nothing


presenceTypeToString :: Maybe PresenceType -> String

presenceTypeToString (Just Unavailable) = "unavailable"
presenceTypeToString (Just Probe) = "probe"
presenceTypeToString Nothing = "error"
presenceTypeToString (Just Subscribe) = "subscribe"
presenceTypeToString (Just Subscribed) = "subscribed"
presenceTypeToString (Just Unsubscribe) = "unsubscribe"
presenceTypeToString (Just Unsubscribed) = "unsubscribed"


stringToMessageType :: String -> Maybe (Maybe MessageType)

stringToMessageType "chat" = Just $ Just Chat
stringToMessageType "error" = Just $ Nothing
stringToMessageType "groupchat" = Just $ Just Groupchat
stringToMessageType "headline" = Just $ Just Headline
stringToMessageType "normal" = Just $ Just Normal
stringToMessageType _ = Nothing


messageTypeToString :: Maybe MessageType -> String

messageTypeToString (Just Chat) = "chat"
messageTypeToString Nothing = "error"
messageTypeToString (Just Groupchat) = "groupchat"
messageTypeToString (Just Headline) = "headline"
messageTypeToString (Just Normal) = "normal"


-- Converts a "<major>.<minor>" numeric version number to a "Version" object.

versionFromString :: String -> Maybe Version

versionFromString s = case parse version "" (DBC.pack s) of
                          Right version -> Just version
                          Left _ -> Nothing


-- Constructs a "Version" based on the major and minor version numbers.

versionFromNumbers :: Integer -> Integer -> Version

versionFromNumbers major minor = Version major minor


version :: GenParser Char st Version

version = do

    -- Read numbers, a dot, more numbers, and end-of-file.
    major <- many1 digit
    char '.'
    minor <- many1 digit
    eof
    return $ Version (read major) (read minor)


-- |
-- Parses, validates, and possibly constructs a "LangTag" object.

langTag :: String -> Maybe LangTag

langTag s = case parse languageTag "" (DBC.pack s) of
                Right tag -> Just tag
                Left _ -> Nothing


-- Parses a language tag as defined by RFC 1766 and constructs a LangTag object.

languageTag :: GenParser Char st LangTag

languageTag = do

    -- Read until we reach a '-' character, or EOF. This is the `primary tag'.
    primTag <- tag

    -- Read zero or more subtags.
    subTags <- subtags
    eof

    return $ LangTag primTag subTags
    where

        subtags :: GenParser Char st [String]
        subtags = many $ do
            char '-'
            subtag <- tag
            return subtag

        tag :: GenParser Char st String
        tag = do
            a <- many1 $ oneOf tagChars
            return a

        tagChars :: [Char]
        tagChars = ['a'..'z'] ++ ['A'..'Z']
