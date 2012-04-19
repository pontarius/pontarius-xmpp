{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Marshalling between XML and Native Types


module Network.XMPP.Pickle where

import Data.XML.Types
import Data.XML.Pickle

import Network.XMPP.Types

import Text.XML.Stream.Elements

mbToBool :: Maybe t -> Bool
mbToBool (Just _) = True
mbToBool _ = False

xpElemEmpty :: Name -> PU [Node] ()
xpElemEmpty name = xpWrap (\((),()) -> ())
                          (\() -> ((),())) $
                              xpElem name xpUnit xpUnit

xmlLang :: Name
xmlLang = Name "lang" Nothing (Just "xml")

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpPrim

-- xpElemExists :: Name -> PU [Node] Bool
-- xpElemExists name = xpWrap (\x -> mbToBool x)
--                            (\x -> if x then Just () else Nothing) $
--                            xpOption (xpElemEmpty name)


xpNodeElem :: PU [Node] a -> PU Element a
xpNodeElem xp = PU { pickleTree = \x -> head $ (pickleTree xp x) >>= \y ->
                      case y of
                        NodeElement e -> [e]
                        _ -> []
             , unpickleTree = \x -> case unpickleTree xp $ [NodeElement x] of
                        Left l -> Left l
                        Right (a,(_,c)) -> Right (a,(Nothing,c))
                   }

ignoreAttrs :: PU t ((), b) -> PU t b
ignoreAttrs = xpWrap snd ((),)

mbl :: Maybe [a] -> [a]
mbl (Just l) = l
mbl Nothing = []

lmb :: [t] -> Maybe [t]
lmb [] = Nothing
lmb x = Just x

right :: Either [Char] t -> t
right (Left l) = error l
right (Right r) = r

unpickleElem' :: PU [Node] c -> Element -> c
unpickleElem' p x = case unpickle (xpNodeElem p) x of
  Left l -> error $ l ++ "\n  saw: " ++ ppElement x
  Right r -> r

unpickleElem :: PU [Node] a -> Element -> Either String a
unpickleElem p x = unpickle (xpNodeElem p) x

pickleElem :: PU [Node] a -> a -> Element
pickleElem p = pickle  $ xpNodeElem p

