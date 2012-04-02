-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
-- Pontarius distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Stream (
-- xmlEnumerator,
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

import Network.XMPP.Types hiding (Continue)

import Prelude hiding (null)

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (null, toChunks)
import Data.Enumerator ((>>==), ($$), Iteratee (..), Enumeratee, Step (..), Enumerator (..), Stream (Chunks), returnI, joinI, run)
import Data.Enumerator.Binary (enumHandle)
import Data.Maybe (fromJust, isJust)
import Data.Text (pack, unpack)
import Data.XML.Types (Content (..), Document (..), Element (..), Event (..), Name (..), Node (..))
import GHC.IO.Handle (Handle)
import Network.TLS (TLSCtx, recvData)
import Text.Parsec (char, count, digit, eof, many, many1, oneOf, parse)
import Text.Parsec.ByteString (GenParser)
-- import Text.XML.Enumerator.Document (fromEvents)
-- import Text.XML.Enumerator.Parse (parseBytes, decodeEntities)

import qualified Data.ByteString as DB (ByteString)
import qualified Data.ByteString.Char8 as DBC (pack)
import qualified Data.Enumerator.List as DEL (head)


-- Reads from the provided handle or TLS context and sends the events to the
-- internal event channel.

-- xmlEnumerator :: Chan InternalEvent -> Either Handle TLSCtx -> IO () -- Was: InternalEvent s m

-- xmlEnumerator c s = do
--     enumeratorResult <- case s of
--         Left handle -> run $ enumHandle 1 handle $$ joinI $
--                        parseBytes decodeEntities $$ eventConsumer c [] 0
--         Right tlsCtx -> run $ enumTLS tlsCtx $$ joinI $
--                         parseBytes decodeEntities $$ eventConsumer c [] 0
--     case enumeratorResult of
--         Right _ -> return () -- writeChan c $ IEE EnumeratorDone
--         Left e -> return () -- writeChan c $ IEE (EnumeratorException e)
--     where
--         -- Behaves like enumHandle, but reads from the TLS context instead
--         -- TODO: Type?
--         enumTLS :: TLSCtx -> Enumerator DB.ByteString IO b
--         enumTLS c s = loop c s
-- 
--         -- TODO: Type?
--         loop :: TLSCtx -> Step DB.ByteString IO b -> Iteratee DB.ByteString IO b
--         loop c (Continue k) = do
--             d <- recvData c
--             case null d of
--                 True  -> loop c (Continue k)
--                 False -> k (Chunks $ toChunks d) >>== loop c
--         loop _ step = returnI step


-- Consumes XML events from the input stream, accumulating as necessary, and
-- sends the proper events through the channel. The second parameter should be
-- initialized to [] (no events) and the third to 0 (zeroth XML level).

-- eventConsumer :: Chan InternalEvent -> [Event] -> Int ->
--                  Iteratee Event IO (Maybe Event) -- Was: InternalEvent s m

-- <stream:stream> open event received.

-- eventConsumer chan [EventBeginElement (Name localName namespace prefixName) attribs] 0
--     | localName == pack "stream" && isJust prefixName && fromJust prefixName == pack "stream" = do
--         liftIO $ return () -- writeChan chan $ IEE $ EnumeratorBeginStream from to id ver lang ns
--         eventConsumer chan [] 1
--     where
--      from = case lookup "from" attribs of Nothing -> Nothing; Just fromAttrib -> Just $ show fromAttrib
--      to = case lookup "to" attribs of Nothing -> Nothing; Just toAttrib -> Just $ show toAttrib
--      id = case lookup "id" attribs of Nothing -> Nothing; Just idAttrib -> Just $ show idAttrib
--      ver = case lookup "version" attribs of Nothing -> Nothing; Just verAttrib -> Just $ show verAttrib
--      lang = case lookup "xml:lang" attribs of Nothing -> Nothing; Just langAttrib -> Just $ show langAttrib
--      ns = case namespace of Nothing -> Nothing; Just namespaceAttrib -> Just $ unpack namespaceAttrib

-- <stream:stream> close event received.

-- eventConsumer chan [EventEndElement name] 1
--  | namePrefix name == Just (pack "stream") && nameLocalName name == pack "stream" = do
--         liftIO $ return () -- writeChan chan $ IEE $ EnumeratorEndStream
--         return Nothing

-- Ignore EventDocumentBegin event.

-- eventConsumer chan [EventBeginDocument] 0 = eventConsumer chan [] 0

-- We have received a complete first-level XML element. Process the accumulated
-- values into an first-level element event.

-- eventConsumer chan ((EventEndElement e):es) 1 = do
--     liftIO $ return () -- writeChan chan $ IEE $ EnumeratorFirstLevelElement $ eventsToElement $ reverse ((EventEndElement e):es)
--     eventConsumer chan [] 1

-- Normal condition - accumulate the event.

-- eventConsumer chan events level = do
--     event <- DEL.head
--     case event of
--         Just event' -> let level' = case event' of
--                                         EventBeginElement _ _ -> level + 1
--                                         EventEndElement _ -> level - 1
--                                         _ -> level
--                        in eventConsumer chan (event':events) level'
--         Nothing -> eventConsumer chan events level


-- eventsToElement :: [Event] -> Either SomeException Element

-- eventsToElement e = do
--     r <- run $ eventsEnum $$ fromEvents
--     case r of Right doc -> Right $ documentRoot doc; Left ex -> Left ex
--     where
--         -- TODO: Type?
--         eventsEnum (Continue k) = k $ Chunks e
--         eventsEnum step = returnI step


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
                  [("type", [ContentText $ pack $ show $ messageType m])]

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
                  stanzaLang ++ [("type", [ContentText $ pack "error"])]

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
        typeAttrib = case presenceType p of Nothing -> []; Just presenceType' -> [("type", [ContentText $ pack $ show presenceType'])]

-- Presence error.

presenceToXML (Left p) streamLang = Element "presence" attribs nodes

    where

        -- Has the stanza attributes and the "error" presence type.
        attribs :: [(Name, [Content])]
        attribs = stanzaAttribs (presenceErrorID p) (presenceErrorFrom p) (presenceErrorTo p)
                  stanzaLang ++ [("type", [ContentText $ pack "error"])]

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
        typeAttrib = [("type", [ContentText $ pack $ show $ iqRequestType i])]

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
        typeAttrib = [("type", [ContentText $ pack "result"])]

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
        typeAttrib = [("type", [ContentText $ pack "error"])]


-- Creates the error element that is common for all stanzas.

errorElem :: LangTag -> Maybe LangTag -> StanzaError -> Element

errorElem streamLang stanzaLang stanzaError = Element "error" typeAttrib
                                              ([defCondElem] ++ textElem ++ appSpecCondElem)

    where

        -- The required stanza error type.
        typeAttrib :: [(Name, [Content])]
        typeAttrib = [("type", [ContentText $ pack $ show $ stanzaErrorType stanzaError])]

        -- The required defined condition element.
        defCondElem :: Node
        defCondElem = NodeElement $ Element (Name (pack $ show $ stanzaErrorCondition stanzaError) (Just $ pack "urn:ietf:params:xml:ns:xmpp-stanzas") Nothing) [] []


        -- The optional text element.
        textElem :: [Node]
        textElem = case stanzaErrorText stanzaError of
                       Nothing -> []
                       Just (textLang, text) ->
                           [NodeElement $ Element "{urn:ietf:params:xml:ns:xmpp-stanzas}text"
                               (langTagAttrib $ childLang streamLang [stanzaLang, fst $ fromJust $ stanzaErrorText stanzaError])
                               [NodeContent $ ContentText $ pack text]]

        -- The optional application specific condition element.
        appSpecCondElem :: [Node]
        appSpecCondElem = case stanzaErrorApplicationSpecificCondition stanzaError of
                              Nothing -> []
                              Just elem -> [NodeElement elem]


-- Generates the element attribute for an optional language tag.

langTagAttrib :: Maybe LangTag -> [(Name, [Content])]

langTagAttrib lang = case lang of Nothing -> []; Just lang' -> [("xml:lang", [ContentText $ pack $ show lang'])]


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

stanzaAttribs i f t l = if isJust $ i then [("id", [ContentText $ pack $ show $ fromJust i])] else [] ++
                      if isJust $ f then [("from", [ContentText $ pack $ show $ fromJust f])] else [] ++
                      if isJust $ t then [("to", [ContentText $ pack $ show $ fromJust t])] else [] ++
                      if isJust $ l then [("xml:lang", [ContentText $ pack $ show l])] else []


parseIQ :: Element -> IQ

parseIQ = parseIQ


parsePresence :: Element -> InternalPresence

parsePresence = parsePresence


parseMessage :: Element -> InternalMessage

parseMessage = parseMessage


-- Converts a string to a PresenceType. Nothing means convertion error, Just
-- Nothing means the presence error type, and Just $ Just is the PresenceType.

stringToPresenceType :: String -> Maybe (Maybe PresenceType)

stringToPresenceType "probe" = Just $ Just Probe
stringToPresenceType "unavailable" = Just $ Just Unavailable
stringToPresenceType "subscribe" = Just $ Just Subscribe
stringToPresenceType "subscribed" = Just $ Just Subscribed
stringToPresenceType "unsubscribe" = Just $ Just Unsubscribe
stringToPresenceType "unsubscribed" = Just $ Just Unsubscribed
stringToPresenceType "error" = Just Nothing
stringToPresenceType _ = Nothing


-- Converts a Maybe MessageType to a string. Nothing means "error".

presenceTypeToString :: Maybe PresenceType -> String

presenceTypeToString (Just Unavailable) = "unavailable"
presenceTypeToString (Just Probe) = "probe"
presenceTypeToString Nothing = "error"
presenceTypeToString (Just Subscribe) = "subscribe"
presenceTypeToString (Just Subscribed) = "subscribed"
presenceTypeToString (Just Unsubscribe) = "unsubscribe"
presenceTypeToString (Just Unsubscribed) = "unsubscribed"


-- Converts a string to a MessageType. Nothing means convertion error, Just
-- Nothing means the message error type, and Just $ Just is the MessageType.

stringToMessageType :: String -> Maybe (Maybe MessageType)

stringToMessageType "chat" = Just $ Just Chat
stringToMessageType "error" = Just $ Nothing
stringToMessageType "groupchat" = Just $ Just Groupchat
stringToMessageType "headline" = Just $ Just Headline
stringToMessageType "normal" = Just $ Just Normal
stringToMessageType _ = Nothing


-- Converts a Maybe MessageType to a string. Nothing means "error".

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
