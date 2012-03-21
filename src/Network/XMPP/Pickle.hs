{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Marshalling between XML and Native Types


module Network.XMPP.Pickle where

import Control.Applicative((<$>))

import qualified Data.ByteString as BS

import Data.Text as Text
import Data.Text.Encoding as Text

import Network.XMPP.Types

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree


mbToBool (Just _) = True
mbToBool _ = False

xpElemEmpty :: Text -> PU [Node Text Text] ()
xpElemEmpty name = xpWrap (\((),()) -> () ,
                              \() -> ((),())) $
                              xpElem name xpUnit xpUnit

xpElemExists :: Text -> PU [Node Text Text] Bool
xpElemExists name = xpWrap (\x -> mbToBool x
                           ,\x -> if x then Just () else Nothing) $
                           xpOption (xpElemEmpty name)


ignoreAttrs :: PU t ((), b) -> PU t b
ignoreAttrs = xpWrap (snd, ((),))

mbl (Just l) = l
mbl Nothing = []

lmb [] = Nothing
lmb x = Just x

right (Left l) = error l
right (Right r) = r


unpickleElem :: PU [Node tag text] c -> Node tag text -> c
unpickleElem p = right . unpickleTree' (xpRoot p)

pickleElem :: PU [Node tag text] a -> a -> Node tag text
pickleElem p = pickleTree $ xpRoot p

xpEither :: PU n t1 -> PU n t2 -> PU n (Either t1 t2)
xpEither l r = xpAlt eitherSel
                   [xpWrap (\x -> Left x, \(Left x) -> x) l
                   ,xpWrap (\x -> Right x, \(Right x) -> x) r
                   ]
  where
    eitherSel (Left _) = 0
    eitherSel (Right _) = 1


xpElemNs ::
     Text
     -> Text
     -> PU [(Text, Text)] t1
     -> PU [Node Text Text] t2
     -> PU [Node Text Text] (t1, t2)
xpElemNs name ns attrs nodes =
   xpWrap (\(((),a),n) -> (a,n), \(a,n) -> (((),a),n)) $
     xpElem name
       (xpPair
         (xpAttrFixed "xmlns" ns)
         attrs
       )
       nodes