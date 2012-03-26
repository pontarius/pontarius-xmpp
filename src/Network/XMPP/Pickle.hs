{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

-- Marshalling between XML and Native Types


module Network.XMPP.Pickle where

import Control.Applicative((<$>))

import qualified Data.ByteString as BS

import qualified Data.Text as Text
import Data.Text.Encoding as Text

import Data.XML.Types
import Data.XML.Pickle

import Network.XMPP.Types



mbToBool (Just _) = True
mbToBool _ = False

xpElemEmpty :: Name -> PU [Node] ()
xpElemEmpty name = xpWrap (\((),()) -> ())
                          (\() -> ((),())) $
                              xpElem name xpUnit xpUnit

-- xpElemExists :: Name -> PU [Node] Bool
-- xpElemExists name = xpWrap (\x -> mbToBool x)
--                            (\x -> if x then Just () else Nothing) $
--                            xpOption (xpElemEmpty name)


xpNodeElem :: PU [Node] a -> PU Element a
xpNodeElem xp = PU { pickleTree = \x -> head $ (pickleTree xp x) >>= \y ->
                      case y of
                        NodeContent _ -> []
                        NodeElement e -> [e]
             , unpickleTree = \x -> case unpickleTree xp $ [NodeElement x] of
                        Left l -> Left l
                        Right (a,(_,c)) -> Right (a,(Nothing,c))
                   }

ignoreAttrs :: PU t ((), b) -> PU t b
ignoreAttrs = xpWrap snd ((),)

mbl (Just l) = l
mbl Nothing = []

lmb [] = Nothing
lmb x = Just x

right (Left l) = error l
right (Right r) = r


unpickleElem :: PU [Node] c -> Element -> c
unpickleElem p = right . unpickle (xpNodeElem p)

pickleElem :: PU [Node] a -> a -> Element
pickleElem p = pickle  $ xpNodeElem p



