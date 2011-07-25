-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

-- The stanza record types are generally pretty convenient to work with.
-- However, due to the fact that an "IQ" can be both an "IQRequest" and an
-- "IQResponse" we provide some helper functions in this module that work on
-- both types.
--
-- We also provide functions to create a new stanza ID generator, and to
-- generate new IDs.

module Network.XMPP.Stanza (
iqID,
iqFrom,
iqTo,
iqLangTag,
iqPayload,
iqPayloadNamespace,
iqRequestPayloadNamespace,
iqResponsePayloadNamespace,
idGenerator,
nextID
) where

import Network.XMPP.Address
import Network.XMPP.Types

import Data.IORef (atomicModifyIORef, newIORef)
import Data.XML.Types (Element, elementName, nameNamespace)
import Data.Text (unpack)


-- |
-- Returns the @StanzaID@ value of the @IQ@, if any.

iqID :: IQ -> Maybe StanzaID

iqID (Left req) = iqRequestID req
iqID (Right res) = iqResponseID res


-- TODO: Maybe?

iqResponseID :: IQResponse -> Maybe StanzaID

iqResponseID (Left err) = iqErrorID err
iqResponseID (Right res) = iqResultID res


-- |
-- Returns the @From@ @JID@ value of the @IQ@, if any.

iqFrom :: IQ -> Maybe From

iqFrom (Left req) = iqRequestFrom req
iqFrom (Right res) = iqResponseFrom res


-- |
-- Returns the @To@ @JID@ value of the @IQ@, if any.

iqTo :: IQ -> Maybe To

iqTo (Left req) = iqRequestTo req
iqTo (Right res) = iqResponseTo res


-- |
-- Returns the @XMLLang@ value of the @IQ@, if any.

iqLangTag :: IQ -> Maybe LangTag

iqLangTag (Left req) = iqRequestLangTag req
iqLangTag (Right res) = iqResponseLangTag res


iqResponseLangTag :: IQResponse -> Maybe LangTag

iqResponseLangTag (Left err) = iqErrorLangTag err
iqResponseLangTag (Right res) = iqResultLangTag res


iqResponseFrom :: IQResponse -> Maybe From

iqResponseFrom (Left err) = iqErrorFrom err
iqResponseFrom (Right res) = iqResultFrom res


iqResponseTo :: IQResponse -> Maybe To

iqResponseTo (Left err) = iqErrorTo err
iqResponseTo (Right res) = iqResultTo res



-- |
-- Returns the @Element@ payload value of the @IQ@, if any. If the IQ in
-- question is of the "request" type, use @iqRequestPayload@ instead.

iqPayload :: IQ -> Maybe Element

iqPayload (Left req) = Just (iqRequestPayload req)
iqPayload (Right res) = iqResponsePayload res


iqResponsePayload :: IQResponse -> Maybe Element

iqResponsePayload (Left err) = iqErrorPayload err
iqResponsePayload (Right res) = iqResultPayload res


-- |
-- Returns the namespace of the element of the @IQ@, if any.

iqPayloadNamespace :: IQ -> Maybe String

iqPayloadNamespace i = case iqPayload i of
  Nothing -> Nothing
  Just p -> case nameNamespace $ elementName p of
    Nothing -> Nothing
    Just n -> Just (unpack n)


-- |
-- Returns the namespace of the element of the @IQRequest@, if any.

iqRequestPayloadNamespace :: IQRequest -> Maybe String

iqRequestPayloadNamespace i = let p = iqRequestPayload i in
  case nameNamespace $ elementName p of
    Nothing -> Nothing
    Just n -> Just (unpack n)


-- |
-- Returns the namespace of the element of the @IQRequest@, if any.

iqResponsePayloadNamespace :: IQResponse -> Maybe String

iqResponsePayloadNamespace i = case iqResponsePayload i of
  Nothing -> Nothing
  Just p -> case nameNamespace $ elementName p of
    Nothing -> Nothing
    Just n -> Just (unpack n)


-- |
-- Creates a new stanza "IDGenerator". Internally, it will maintain an infinite
-- list of stanza IDs ('[\'a\', \'b\', \'c\'...]').

idGenerator :: String -> IO IDGenerator

idGenerator p = newIORef (ids p) >>= \ ioRef -> return $ IDGenerator ioRef


-- |
-- Extracts an ID from the "IDGenerator", and updates the generators internal
-- state so that the same ID will not be generated again.

nextID :: IDGenerator -> IO String

nextID g = let IDGenerator ioRef = g
           in atomicModifyIORef ioRef (\ (i:is) -> (is, i))


-- Generates an infinite and predictable list of IDs, all beginning with the
-- provided prefix.

ids :: String -> [String]

-- Adds the prefix to all combinations of IDs (ids').
ids p = map (\ id -> p ++ id) ids'
    where

        -- Generate all combinations of IDs, with increasing length.
        ids' :: [String]
        ids' = concatMap ids'' [1..]

        -- Generates all combinations of IDs with the given length.
        ids'' :: Integer -> [String]
        ids'' 0 = [""]
        ids'' l = [x:xs | x <- repertoire, xs <- ids'' (l - 1)]

        -- Characters allowed in IDs.
        repertoire :: String
        repertoire = ['a'..'z']
