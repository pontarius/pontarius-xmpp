{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Xml.Stream.Elements where

import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource as R

import qualified Data.ByteString as BS
import           Data.Conduit as C
import           Data.Conduit.List as CL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable
import           Data.XML.Types

import           System.IO.Unsafe(unsafePerformIO)

import qualified Text.XML.Stream.Render as TXSR
import           Text.XML.Unresolved as TXU

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `Text.append` y) : z
compressNodes (x:xs) = x : compressNodes xs

streamName :: Name
streamName =
    (Name "stream" (Just "http://etherx.jabber.org/streams") (Just "stream"))

data StreamEnd = StreamEnd deriving (Typeable, Show)
instance Exception StreamEnd

data InvalidXmppXml = InvalidXmppXml String deriving (Show, Typeable)

instance Exception InvalidXmppXml

parseElement txt = documentRoot $ TXU.parseText_ TXU.def txt

elements :: R.MonadThrow m => C.Conduit Event m Element
elements = do
        x <- C.await
        case x of
            Just (EventBeginElement n as) -> do
                                                 goE n as >>= C.yield
                                                 elements
            Just (EventEndElement streamName) -> lift $ R.monadThrow StreamEnd
            Nothing -> return ()
            _ -> lift $ R.monadThrow $ InvalidXmppXml $ "not an element: " ++ show x
  where
    many' f =
        go id
      where
        go front = do
            x <- f
            case x of
                Left x -> return $ (x, front [])
                Right y -> go (front . (:) y)
    goE n as = do
        (y, ns) <- many' goN
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ R.monadThrow $ InvalidXmppXml $
                                         "Missing close tag: " ++ show n
    goN = do
        x <- await
        case x of
            Just (EventBeginElement n as) -> (Right . NodeElement) <$> goE n as
            Just (EventInstruction i) -> return $ Right $ NodeInstruction i
            Just (EventContent c) -> return $ Right $ NodeContent c
            Just (EventComment t) -> return $ Right $ NodeComment t
            Just (EventCDATA t) -> return $ Right $ NodeContent $ ContentText t
            _ -> return $ Left x


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

elementToEvents :: Element -> [Event]
elementToEvents e@(Element name _ _) = openElementToEvents e ++ [EventEndElement name]


renderOpenElement :: Element -> BS.ByteString
renderOpenElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (openElementToEvents e) $$ TXSR.renderText def =$ CL.consume

renderElement :: Element -> BS.ByteString
renderElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (elementToEvents e) $$ TXSR.renderText def =$ CL.consume

ppElement :: Element -> String
ppElement = Text.unpack . Text.decodeUtf8 . renderElement