{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Utilities
    ( presTo
    , message
    , answerMessage
    , openElementToEvents
    , renderOpenElement
    , renderElement
    , checkHostName
    )
    where

import           Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import           Data.Conduit as C
import           Data.Conduit.List as CL
import qualified Data.Text as Text
import           Data.Text(Text)
import qualified Data.Text.Encoding as Text
import           Data.XML.Types
import           Network.Xmpp.Types
import           Prelude
import           System.IO.Unsafe(unsafePerformIO)
import qualified Text.XML.Stream.Render as TXSR
import           Text.XML.Unresolved as TXU

-- | Add a recipient to a presence notification.
presTo :: Presence -> Jid -> Presence
presTo pres to = pres{presenceTo = Just to}

-- | An empty message.
message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  }

-- | Produce an answer message with the given payload, switching the "from" and
-- "to" attributes in the original message. Produces a 'Nothing' value of the
-- provided message message has no from attribute.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload =
    Just Message{ messageFrom    = messageTo
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload
                , ..
                }
answerMessage _ _ = Nothing

openElementToEvents :: Element -> [Event]
openElementToEvents (Element name as ns) = EventBeginElement name as : goN ns []
  where
    goE (Element name' as' ns') =
          (EventBeginElement name' as' :)
        . goN ns'
        . (EventEndElement name' :)
    goN [] = id
    goN [x] = goN' x
    goN (x:xs) = goN' x . goN xs
    goN' (NodeElement e) = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent c) = (EventContent c :)
    goN' (NodeComment t) = (EventComment t :)

renderOpenElement :: Element -> BS.ByteString
renderOpenElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (openElementToEvents e) $$ TXSR.renderText def =$ CL.consume

renderElement :: Element -> BS.ByteString
renderElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (elementToEvents e) $$ TXSR.renderText def =$ CL.consume
  where
    elementToEvents :: Element -> [Event]
    elementToEvents el@(Element name _ _) = openElementToEvents el
                                              ++ [EventEndElement name]

-- | Validates the hostname string in accordance with RFC 1123.
checkHostName :: Text -> Maybe Text
checkHostName t = do
    eitherToMaybeHostName $ AP.parseOnly hostnameP t
  where
    eitherToMaybeHostName = either (const Nothing) Just

-- Validation of RFC 1123 hostnames.
hostnameP :: AP.Parser Text
hostnameP = do
    -- Hostnames may not begin with a hyphen.
    h <- AP.satisfy $ AP.inClass $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    t <- AP.takeWhile $ AP.inClass $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['-']
    let label = Text.concat [Text.pack [h], t]
    if Text.length label > 63
        then fail "Label too long."
        else do
            AP.endOfInput
            return label
            <|> do
                _ <- AP.satisfy (== '.')
                r <- hostnameP
                if (Text.length label) + 1 + (Text.length r) > 255
                    then fail "Hostname too long."
                    else return $ Text.concat [label, Text.pack ".", r]
