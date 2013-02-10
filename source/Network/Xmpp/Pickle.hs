{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Marshalling between XML and Native Types


module Network.Xmpp.Pickle
    ( xmlLang
    , xpLangTag
    , unpickleElem'
    , unpickleElem
    , pickleElem
    )
    where

import Data.XML.Types
import Data.XML.Pickle

import Network.Xmpp.Types

import Text.Xml.Stream.Elements

xmlLang :: Name
xmlLang = Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpPrim

-- Given a pickler and an element, produces an object.
unpickleElem :: PU [Node] a -> Element -> Either UnpickleError a
unpickleElem p x = unpickle p [NodeElement x]

unpickleElem' :: PU [Node] c -> Element -> c
unpickleElem' p x = case unpickleElem p x of
  Left l -> error $ (show l) ++ "\n  saw: " ++ ppElement x
  Right r -> r

-- Given a pickler and an object, produces an Element.
pickleElem :: PU [Node] a -> a -> Element
pickleElem p x = case pickle p x of
    [NodeElement e] -> e
    _ -> error "pickleElem: Pickler didn't return a single element."
