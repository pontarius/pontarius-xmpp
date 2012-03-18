module Text.XML.Stream.Elements where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Class

import Data.Text as T
import Text.XML.Unresolved
import Data.XML.Types

import Data.Conduit as C
import Data.Conduit.List as CL

import Text.XML.Stream.Parse

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `T.append` y) : z
compressNodes (x:xs) = x : compressNodes xs

elementFromEvents :: C.ResourceThrow m => C.Sink Event m Element
elementFromEvents = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> goE n as
            _ -> lift $ C.resourceThrow $ InvalidEventStream $ "not an element: " ++ show x
  where
    many f =
        go id
      where
        go front = do
            x <- f
            case x of
                Nothing -> return $ front []
                Just y -> go (front . (:) y)
    dropReturn x = CL.drop 1 >> return x
    goE n as = do
        CL.drop 1
        ns <- many goN
        y <- CL.head
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ C.resourceThrow $ InvalidEventStream $ "Missing end element for " ++ show n ++ ", got: " ++ show y
    goN = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> (Just . NodeElement) <$> goE n as
            Just (EventInstruction i) -> dropReturn $ Just $ NodeInstruction i
            Just (EventContent c) -> dropReturn $ Just $ NodeContent c
            Just (EventComment t) -> dropReturn $ Just $ NodeComment t
            Just (EventCDATA t) -> dropReturn $ Just $ NodeContent $ ContentText t
            _ -> return Nothing


elementToEvents' :: Element -> [Event]
elementToEvents' (Element name as ns) = EventBeginElement name as : goN ns []
  where
    goM [] = id
    goM [x] = (goM' x :)
    goM (x:xs) = (goM' x :) . goM xs
    goM' (MiscInstruction i) = EventInstruction i
    goM' (MiscComment t) = EventComment t
    goE (Element name as ns) =
          (EventBeginElement name as :)
        . goN ns
        . (EventEndElement name :)
    goN [] = id
    goN [x] = goN' x
    goN (x:xs) = goN' x . goN xs
    goN' (NodeElement e) = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent c) = (EventContent c :)
    goN' (NodeComment t) = (EventComment t :)

elementToEvents e@(Element name _ _) = elementToEvents' e ++ [EventEndElement name]
