-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Stream (
isTLSSecured,
xmlEnumerator,
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
import Network.XMPP.Types hiding (Continue)
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

import Data.Enumerator ((>>==), Iteratee (..), Enumeratee, Step (..), Enumerator (..), Stream (Chunks), returnI)
import qualified Data.Enumerator.List as DEL (head)

import Control.Exception.Base (SomeException)


isTLSSecured :: TLSState -> Bool
isTLSSecured (PostHandshake _) = True
isTLSSecured _ = False


-- Reads from the provided handle or TLS context and sends the events to the
-- internal event channel.

xmlEnumerator :: Chan (InternalEvent s m) -> Either Handle TLSCtx -> IO ()
xmlEnumerator c s = do
  enumeratorResult <- case s of
    Left handle -> run $ enumHandle 1 handle $$ joinI $
                   parseBytes decodeEntities $$ eventConsumer c [] 0
    Right tlsCtx -> run $ enumTLS tlsCtx $$ joinI $
                    parseBytes decodeEntities $$ eventConsumer c [] 0
  case enumeratorResult of
    Right _ ->
      writeChan c $ IEE EnumeratorDone
    Left e ->
      writeChan c $ IEE (EnumeratorException e)
  where
    -- Behaves like enumHandle, but reads from the TLS context instead
    enumTLS :: TLSCtx -> Enumerator DB.ByteString IO b
    enumTLS c s = loop c s

    loop :: TLSCtx -> Step DB.ByteString IO b -> Iteratee DB.ByteString IO b
    loop c (Continue k) = do
      d <- recvData c
      case DBL.null d of
        True  -> loop c (Continue k)
        False -> k (Chunks $ DBL.toChunks d) >>== loop c
    loop _ step = returnI step


-- Consumes XML events from the input stream, accumulating as necessary, and
-- sends the proper events through the channel. The second parameter should be
-- initialized to [] (no events) and the third to 0 (zeroth XML level).

eventConsumer :: Chan (InternalEvent s m) -> [Event] -> Int ->
                 Iteratee Event IO (Maybe Event)

-- <stream:stream> open event received.

eventConsumer chan [EventBeginElement (Name localName namespace prefixName) attribs] 0
    | localName == DT.pack "stream" && isJust prefixName && fromJust prefixName == DT.pack "stream" = do
        liftIO $ writeChan chan $ IEE $ EnumeratorBeginStream from to id ver lang ns
        eventConsumer chan [] 1
    where
        from = case lookup "from" attribs of Nothing -> Nothing; Just fromAttrib -> Just $ show fromAttrib
        to = case lookup "to" attribs of Nothing -> Nothing; Just toAttrib -> Just $ show toAttrib
        id = case lookup "id" attribs of Nothing -> Nothing; Just idAttrib -> Just $ show idAttrib
        ver = case lookup "version" attribs of Nothing -> Nothing; Just verAttrib -> Just $ show verAttrib
        lang = case lookup "xml:lang" attribs of Nothing -> Nothing; Just langAttrib -> Just $ show langAttrib
        ns = case namespace of Nothing -> Nothing; Just namespaceAttrib -> Just $ DT.unpack namespaceAttrib

-- <stream:stream> close event received.

eventConsumer chan [EventEndElement name] 1
    | namePrefix name == Just (DT.pack "stream") && nameLocalName name == DT.pack "stream" = do
        liftIO $ writeChan chan $ IEE $ EnumeratorEndStream
        return Nothing

-- Ignore EventDocumentBegin event.

eventConsumer chan [EventBeginDocument] 0 = eventConsumer chan [] 0

-- We have received a complete first-level XML element. Process the accumulated
-- values into an first-level element event.

eventConsumer chan ((EventEndElement e):es) 1 = do
    liftIO $ writeChan chan $ IEE $ EnumeratorFirstLevelElement $ eventsToElement $ reverse ((EventEndElement e):es)
    eventConsumer chan [] 1

-- Normal condition - accumulate the event.

eventConsumer chan events level = do
    event <- DEL.head
    case event of
        Just event' -> let level' = case event' of
                                        EventBeginElement _ _ -> level + 1
                                        EventEndElement _ -> level - 1
                                        _ -> level
                       in eventConsumer chan (event':events) level'
        Nothing -> eventConsumer chan events level


eventsToElement :: [Event] -> Either SomeException Element

eventsToElement e = do
    r <- run $ eventsEnum $$ fromEvents
    case r of Right doc -> Right $ documentRoot doc; Left ex -> Left ex
    where
        -- TODO: Type?
        eventsEnum (Continue k) = k $ Chunks e
        eventsEnum step = returnI step


-- Sending stanzas is done through functions, where LangTag is Maybe.


-- Generates an XML element for a message stanza. The language tag provided is
-- the default language of the stream.

messageToXML :: InternalMessage -> LangTag -> Element

-- Non-error message.

messageToXML (Right m) streamLang = Element "message" attribs nodes

    where

        -- Has the stanza attributes and the message type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (messageID m) (messageFrom m) (messageTo m) stanzaLang ++
                  [("type", [ContentText $ DT.pack $ show $ messageType m])]

        -- Has an arbitrary number of elements as children.
        nodes :: [Node]
        nodes = map (\ x -> NodeElement x) (messagePayload m)

        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ messageLangTag m

-- Presence error.

messageToXML (Left m) streamLang = Element "message" attribs nodes

    where

        -- Has the stanza attributes and the "error" presence type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (messageErrorID m) (messageErrorFrom m) (messageErrorTo m)
                  stanzaLang ++ [("type", [ContentText $ DT.pack "error"])]

        -- Has the error element stanza as its child.
        -- TODO: Include sender XML here?
        nodes :: [Node]
        nodes = [NodeElement $ errorElem streamLang stanzaLang $ messageErrorStanzaError m]

        -- The stanza language tag, if it's different from the stream language tag.
        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ messageErrorLangTag m


-- Generates an XML element for a presence stanza. The language tag provided is
-- the default language of the stream.

presenceToXML :: InternalPresence -> LangTag -> Element

-- Non-error presence.

presenceToXML (Right p) streamLang = Element "presence" attribs nodes

    where

        -- Has the stanza attributes and the presence type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (presenceID p) (presenceFrom p) (presenceTo p) stanzaLang ++
                  typeAttrib

        -- Has an arbitrary number of elements as children.
        nodes :: [Node]
        nodes = map (\ x -> NodeElement x) (presencePayload p)

        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ presenceLangTag p

        typeAttrib :: [(Name, [Content])]
        typeAttrib = case presenceType p of Nothing -> []; Just presenceType' -> [("type", [ContentText $ DT.pack $ show presenceType'])]

-- Presence error.

presenceToXML (Left p) streamLang = Element "presence" attribs nodes

    where

        -- Has the stanza attributes and the "error" presence type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (presenceErrorID p) (presenceErrorFrom p) (presenceErrorTo p)
                  stanzaLang ++ [("type", [ContentText $ DT.pack "error"])]

        -- Has the error element stanza as its child.
        -- TODO: Include sender XML here?
        nodes :: [Node]
        nodes = [NodeElement $ errorElem streamLang stanzaLang $ presenceErrorStanzaError p]

        -- The stanza language tag, if it's different from the stream language tag.
        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ presenceErrorLangTag p


-- Generates an XML element for a presence stanza. The language tag provided is
-- the default language of the stream.

iqToXML :: IQ -> LangTag -> Element

-- Request IQ.

iqToXML (Left i) streamLang = Element "iq" attribs nodes

    where

        -- Has the stanza attributes and the IQ request type (`get' or `set').
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (iqRequestID i) (iqRequestFrom i) (iqRequestTo i)
                  stanzaLang ++ typeAttrib

        -- Has exactly one payload child element.
        nodes :: [Node]
        nodes = [NodeElement $ iqRequestPayload i]

        -- The stanza language tag, if it's different from the stream language tag.
        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ iqRequestLangTag i

        -- The required type attribute.
        typeAttrib :: [(Name, [Content])]
        typeAttrib = [("type", [ContentText $ DT.pack $ show $ iqRequestType i])]

-- Response result IQ.

iqToXML (Right (Right i)) streamLang = Element "iq" attribs nodes

    where

        -- Has the stanza attributes and the IQ `result' type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (iqResultID i) (iqResultFrom i) (iqResultTo i)
                  stanzaLang ++ typeAttrib

        -- Has one or zero payload child elements.
        nodes :: [Node]
        nodes = case iqResultPayload i of Nothing -> []; Just payloadElem -> [NodeElement payloadElem]

        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ iqResultLangTag i

        -- The required type attribute.
        typeAttrib :: [(Name, [Content])]
        typeAttrib = [("type", [ContentText $ DT.pack "result"])]

-- Response error IQ.

iqToXML (Right (Left i)) streamLang = Element "iq" attribs nodes

    where

        -- Has the stanza attributes and the presence type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (iqErrorID i) (iqErrorFrom i) (iqErrorTo i) stanzaLang ++
                  typeAttrib

        -- Has an optional elements as child.
        nodes :: [Node]
        nodes = case iqErrorPayload i of Nothing -> []; Just payloadElem -> [NodeElement payloadElem]

        stanzaLang :: Maybe LangTag
        stanzaLang = stanzaLang' streamLang $ iqErrorLangTag i

        typeAttrib :: [(Name, [Content])]
        typeAttrib = [("type", [ContentText $ DT.pack "error"])]


-- Creates the error element that is common for all stanzas.

errorElem :: LangTag -> Maybe LangTag -> StanzaError -> Element

errorElem streamLang stanzaLang stanzaError = Element "error" typeAttrib
                                              ([defCondElem] ++ textElem ++ appSpecCondElem)

    where

        -- The required stanza error type.
        typeAttrib :: [(Name, [Content])]
        typeAttrib = [("type", [ContentText $ DT.pack $ show $ stanzaErrorType stanzaError])]

        -- The required defined condition element.
        defCondElem :: Node
        defCondElem = NodeElement $ Element (Name (DT.pack $ show $ stanzaErrorCondition stanzaError) (Just $ DT.pack "urn:ietf:params:xml:ns:xmpp-stanzas") Nothing) [] []


        -- The optional text element.
        textElem :: [Node]
        textElem = case stanzaErrorText stanzaError of
                       Nothing -> []
                       Just (textLang, text) ->
                           [NodeElement $ Element "{urn:ietf:params:xml:ns:xmpp-stanzas}text"
                               (langTagAttrib $ childLang streamLang [stanzaLang, fst $ fromJust $ stanzaErrorText stanzaError])
                               [NodeContent $ ContentText $ DT.pack text]]

        -- The optional application specific condition element.
        appSpecCondElem :: [Node]
        appSpecCondElem = case stanzaErrorApplicationSpecificCondition stanzaError of
                              Nothing -> []
                              Just elem -> [NodeElement elem]


-- Generates the element attribute for an optional language tag.

langTagAttrib :: Maybe LangTag -> [(Name, [Content])]

langTagAttrib lang = case lang of Nothing -> []; Just lang' -> [("xml:lang", [ContentText $ DT.pack $ show lang'])]


stanzaLang' :: LangTag -> LangTag -> Maybe LangTag

stanzaLang' streamLang stanzaLang | streamLang == stanzaLang = Nothing
                                  | otherwise = Just stanzaLang


-- Finds the language tag to set on the current element, if any. Makes sure that
-- language tags are not repeated unnecessarily (like on a child element, when
-- the parent has it). The first parameter is the stream language tag, and the
-- list of optional language tags are ordered in their XML element child
-- sequence, parent first, starting with the stanza language tag.

childLang :: LangTag -> [Maybe LangTag] -> Maybe LangTag

childLang streamLang optLangTags

    -- The current element does not have a language tag - set nothing.
    | (head $ reverse optLangTags) == Nothing = Nothing

    -- All optional language tags are Nothing - set nothing.
    | length langTags == 1 = Nothing

    -- The language tag of this element is the same as the closest parent with a
    -- language tag - set nothing.
    | (head langTags) == (head $ tail langTags) = Nothing

    -- Set the language tag.
    | otherwise = Just $ head langTags

    where

        -- Contains the chain of language tags in descending priority order.
        -- Contains at least one element - the stream language tag.
        langTags = reverse $ [streamLang] ++ (map fromJust $ filter (\ l -> isJust l) optLangTags)


-- Creates the attributes common for all stanzas.

stanzaAttribs :: Maybe StanzaID -> Maybe From -> Maybe To -> Maybe LangTag -> [(Name, [Content])]

stanzaAttribs i f t l = if isJust $ i then [("id", [ContentText $ DT.pack $ show $ fromJust i])] else [] ++
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
