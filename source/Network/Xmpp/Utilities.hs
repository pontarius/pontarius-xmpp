{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}



module Network.Xmpp.Utilities
    ( openElementToEvents
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
import           Prelude
import           System.IO.Unsafe(unsafePerformIO)
import qualified Text.XML.Stream.Render as TXSR
import           Text.XML.Unresolved as TXU

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
checkHostName t =
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
                if Text.length label + 1 + Text.length r > 255
                    then fail "Hostname too long."
                    else return $ Text.concat [label, Text.pack ".", r]
