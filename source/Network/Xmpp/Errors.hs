{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Errors where

import           Control.Applicative ((<$>))
import           Control.Monad(unless)
import           Control.Monad.Error
import           Control.Monad.Error.Class
import qualified Data.Text as Text
import           Data.XML.Types
import           Network.Xmpp.Types
import           Network.Xmpp.Pickle


-- Finds unpickling problems. Not to be used for data validation
findStreamErrors :: Element -> StreamError
findStreamErrors (Element name attrs children)
    | (nameLocalName name /= "stream")
        = StreamNotStreamElement $ nameLocalName name
    | (nameNamespace name /= Just "http://etherx.jabber.org/streams")
        = StreamInvalidStreamNamespace  $ nameNamespace name
    | otherwise = checkchildren (flattenAttrs attrs)
  where
    checkchildren children =
        let to'  = lookup "to"      children
            ver' = lookup "version" children
            xl   = lookup xmlLang   children
          in case () of () | Just (Nothing :: Maybe Jid) == (safeRead <$> to')
                             -> StreamWrongTo to'
                           | Nothing == ver'
                             -> StreamWrongVersion Nothing
                           | Just (Nothing :: Maybe LangTag) ==
                             (safeRead <$> xl)
                             -> StreamWrongLangTag xl
                           | otherwise
                             -> StreamUnknownError
    safeRead x = case reads $ Text.unpack x of
        [] -> Nothing
        ((y,_):_) -> Just y

flattenAttrs :: [(Name, [Content])] -> [(Name, Text.Text)]
flattenAttrs attrs = map (\(name, content) ->
                             ( name
                             , Text.concat $ map uncontentify content)
                             )
                         attrs
  where
    uncontentify (ContentText t) = t
    uncontentify _ = ""
