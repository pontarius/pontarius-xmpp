{-# Language OverloadedStrings, ViewPatterns, NoMonomorphismRestriction #-}

module Network.XMPP.Marshal where

import Control.Applicative((<$>))

import Control.Monad.State

import Data.Maybe

import qualified Data.Text as Text
import Data.XML.Types

import Network.XMPP.Types


-- create attribute from Just
matr _ Nothing = []
matr n (Just x) = [(n,x)]

-- Child if text is not empty
nech _ "" = []
nech n x  = [ NodeElement (Element n [] [NodeContent (ContentText x) ]) ]

-- Child if text is not Nothing
mnech _ Nothing = []
mnech n (Just x) = [ NodeElement (Element n [] [NodeContent (ContentText x) ]) ]

-- make Attributes from text
contentify (x,y) = (x, [ContentText y])

-- Marshal Message to XML Element
messageToElement (Message from to ident tp sub body thread exts) =
  Element "message"
    (map contentify . concat $
      [ matr "from" (toText <$> from)
      , [("to", toText to)]
      , matr "id"    ident
      , [("type", toText tp)]
      ])
    (concat $
      [ mnech "subject" sub
      , mnech "body"    body
      , mnech "thread"  thread
      ,  map NodeElement exts
      ])

-- Marshal XML element to message
elementToMessage e@(Element "message" _ _) =
  let from    = fromText <$> attributeText "from" e
      Just to = fromText <$> attributeText "to"   e
      ident   = attributeText "id"   e
      Just tp = fromText <$> attributeText "type" e
      -- Oh dear, this is HORRIBLE. TODO: come up with something sane
      in grabFrom (elementChildren e) $ do
        -- TODO multiple bodies (different languages)
        body <- maybeGrabNamed "body"
        -- TODO multiple subjects (different languages)
        subject <-  maybeGrabNamed "subject"
        thread <- maybeGrabNamed "thread"
        ext <- grabRest
        return $ Message
                   from
                   to
                   ident
                   tp
                   (elementToText <$>subject)
                   (elementToText <$> body)
                   (elementToText <$> thread)
                   ext

presenceTOXML (Presence from to id tp stp stat pri exts) =
  Element "message"
    (map contentify . concat $
      [ matr "from" (toText <$> from)
      , matr "to"  (toText <$> to)
      , matr "id" id
      , matr "type" ( toText <$> tp)
      ])
    (concat $
      [ mnech "show"    (toText <$> stp)
      , mnech "status"  stat
      , mnech "priority"  (Text.pack . show <$> pri)
      , map NodeElement exts
      ])

-- Marshal XML element to message
elementToPresence e@(Element "message" _ _) =
  let from    = fromText <$> attributeText "from" e
      to      = fromText <$> attributeText "to"   e
      ident   = attributeText "id"   e
      tp = fromText <$> attributeText "type" e
      in grabFrom (elementChildren e) $ do
        pshow <- maybeGrabNamed "show"
        -- TODO multiple status (different languages)
        stat  <-  maybeGrabNamed "status"
        prio  <-  maybeGrabNamed "priority"
        ext   <- grabRest
        return $ Presence
                   from
                   to
                   ident
                   tp
                   (fromText . elementToText <$> pshow)
                   (elementToText <$> stat)
                   (read . Text.unpack . elementToText <$> prio)
                   ext


iqToElement (IQ from to id tp body) =
  Element "message"
    (map contentify . concat $
      [ matr "from" (toText <$> from)
      , matr "to"   (toText <$> to  )
      , [("id" , id)]
      , [("type", toText tp)]
      ])
    [ NodeElement body ]

elementToIQ e@(Element "iq" _ _) =
  let from      = fromText <$> attributeText "from" e
      to        = fromText <$> attributeText "to"   e
      Just ident= attributeText "id"   e
      Just tp = fromText <$> attributeText "type" e
      [ext] = elementChildren e
   in IQ
        from
        to
        ident
        tp
        ext

-- take and remove all elements matching a predicate from the list
takeAllFromList pred l = let (l', xs) = go pred [] l in (reverse l', xs)
  where
  go pred ys  [] = (ys, [])
  go pred ys (x:xs) =
    case pred x of
      True -> let (ys', rs) = go pred ys xs in (ys', x:rs)
      False -> go pred (x:ys) xs

-- The "Grab Monad" : sucessively take and remove ("grab")
-- elements from a "pool" (list)

-- Put a list of elements into the pool and start grabbing
grabFrom l = flip runState l

-- grab all elements matching predicate out of the pool
grabAll p = do
  l <- get
  let (l', xs) = takeAllFromList p l
  put l'
  return xs

-- grab XML-elements by exact name
grabNamed = grabAll . hasName

-- This throws away all elements after the first one
-- TODO: Be more stricy here
maybeGrabNamed = liftM listToMaybe . grabAll . hasName

-- grab all remaining elements from the pool
grabRest = do
  l <- get
  put []
  return l

hasName x e = x == elementName e

elementToText = Text.concat . elementText