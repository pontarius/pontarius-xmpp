{-

Copyright © 2010-2011 Jon Kristensen.

This file is part of Pontarius XMPP.

Pontarius XMPP is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Pontarius XMPP is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with Pontarius XMPP. If not, see <http://www.gnu.org/licenses/>.

-}


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
iqXMLLang,
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

iqID (IQReq i) = iqRequestID i
iqID (IQRes i) = iqResponseID i


-- |
-- Returns the @From@ @JID@ value of the @IQ@, if any.

iqFrom :: IQ -> Maybe From

iqFrom (IQReq i) = iqRequestFrom i
iqFrom (IQRes i) = iqResponseFrom i


-- |
-- Returns the @To@ @JID@ value of the @IQ@, if any.

iqTo :: IQ -> Maybe To

iqTo (IQReq i) = iqRequestTo i
iqTo (IQRes i) = iqResponseTo i


-- |
-- Returns the @XMLLang@ value of the @IQ@, if any.

iqXMLLang :: IQ -> Maybe XMLLang

iqXMLLang (IQReq i) = iqRequestXMLLang i
iqXMLLang (IQRes i) = iqResponseXMLLang i


-- |
-- Returns the @Element@ payload value of the @IQ@, if any. If the IQ in
-- question is of the "request" type, use @iqRequestPayload@ instead.

iqPayload :: IQ -> Maybe Element

iqPayload (IQReq i) = Just (iqRequestPayload i)
iqPayload (IQRes i) = iqResponsePayload i


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
