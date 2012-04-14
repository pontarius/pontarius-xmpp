module Text.XML.Stream.Elements where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource as R

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.XML.Types
import qualified Text.XML.Stream.Render as TXSR
import           Text.XML.Unresolved as TXU

import           Data.Conduit as C
import           Data.Conduit.List as CL

import           System.IO.Unsafe(unsafePerformIO)

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `Text.append` y) : z
compressNodes (x:xs) = x : compressNodes xs

elementFromEvents :: R.MonadThrow m => C.Sink Event m Element
elementFromEvents = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> goE n as
            _ -> lift $ R.monadThrow $ InvalidEventStream $ "not an element: " ++ show x
  where
    many' f =
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
        ns <- many' goN
        y <- CL.head
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ R.monadThrow $ InvalidEventStream $ "Missing end element for " ++ show n ++ ", got: " ++ show y
    goN = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> (Just . NodeElement) <$> goE n as
            Just (EventInstruction i) -> dropReturn $ Just $ NodeInstruction i
            Just (EventContent c) -> dropReturn $ Just $ NodeContent c
            Just (EventComment t) -> dropReturn $ Just $ NodeComment t
            Just (EventCDATA t) -> dropReturn $ Just $ NodeContent $ ContentText t
            _ -> return Nothing


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