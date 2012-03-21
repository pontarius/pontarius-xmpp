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

xpElemEmpty name = xpWrap (\((),()) -> () ,
                              \() -> ((),())) $
                              xpElem name xpUnit xpUnit

xpElemExists name = xpWrap (\x -> mbToBool x
                           ,\x -> if x then Just () else Nothing) $
                           xpOption (xpElemEmpty name)

ignoreAttrs = xpWrap (snd, ((),))

mbl (Just l) = l
mbl Nothing = []

lmb [] = Nothing
lmb x = Just x

right (Left l) = error l
right (Right r) = r

unpickleElem p = right . unpickleTree' (xpRoot p)
pickleElem p = pickleTree $ xpRoot p

xpEither l r = xpAlt eitherSel
                   [xpWrap (\x -> Left x, \(Left x) -> x) l
                   ,xpWrap (\x -> Right x, \(Right x) -> x) r
                   ]
  where
    eitherSel (Left _) = 0
    eitherSel (Right _) = 1



xpElemNs name ns attrs nodes =
   xpWrap (\(((),a),n) -> (a,n), \(a,n) -> (((),a),n)) $
     xpElem name
       (xpPair
         (xpAttrFixed "xmlns" ns)
         attrs
       )
       nodes