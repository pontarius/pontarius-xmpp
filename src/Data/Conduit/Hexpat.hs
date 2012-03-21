{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

module Data.Conduit.Hexpat where

import Control.Applicative((<$>))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import qualified Data.ByteString as BS
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Maybe
import Data.Typeable

import Text.XML.Expat.Internal.IO hiding (parse)
import Text.XML.Expat.SAX
import Text.XML.Expat.Tree

import Foreign.Ptr

import Data.IORef
-- adapted from parseG

-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.
-- parseG :: forall tag text l . (GenericXMLString tag, GenericXMLString text, List l) =>
--           ParseOptions tag text -- ^ Parse options
--        -> l ByteString          -- ^ Input text (a lazy ByteString)
--        -> l (SAXEvent tag text)
-- parseG opts inputBlocks = runParser inputBlocks parser queueRef cacheRef
--   where

data HexpatParser tag text a = HexpatParser
                             { hParser :: Parser
                             , hQueueRef ::  IORef [SAXEvent tag text]
                             }

createParser
  :: (GenericXMLString tag, GenericXMLString text) =>
     ParseOptions tag text -> IO (HexpatParser tag text a)
createParser opts =  do
        let enc = overrideEncoding opts
        let mEntityDecoder = entityDecoder opts

        parser <- newParser enc
        queueRef <- newIORef []

        case mEntityDecoder of
            Just deco -> setEntityDecoder parser deco $ \_ txt -> do
                modifyIORef queueRef (CharacterData txt:)
            Nothing -> return ()

        setXMLDeclarationHandler parser $ \_ cVer cEnc cSd -> do
            ver <- textFromCString cVer
            mEnc <- if cEnc == nullPtr
                then return Nothing
                else Just <$> textFromCString cEnc
            let sd = if cSd < 0
                    then Nothing
                    else Just $ if cSd /= 0 then True else False
            modifyIORef queueRef (XMLDeclaration ver mEnc sd:)
            return True

        setStartElementHandler parser $ \_ cName cAttrs -> do
            name <- textFromCString cName
            attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
                attrName <- textFromCString cAttrName
                attrValue <- textFromCString cAttrValue
                return (attrName, attrValue)
            modifyIORef queueRef (StartElement name attrs:)
            return True

        setEndElementHandler parser $ \_ cName -> do
            name <- textFromCString cName
            modifyIORef queueRef (EndElement name:)
            return True

        setCharacterDataHandler parser $ \_ cText -> do
            txt <- gxFromCStringLen cText
            modifyIORef queueRef (CharacterData txt:)
            return True

        setStartCDataHandler parser $ \_  -> do
            modifyIORef queueRef (StartCData :)
            return True

        setEndCDataHandler parser $ \_  -> do
            modifyIORef queueRef (EndCData :)
            return True

        setProcessingInstructionHandler parser $ \_ cTarget cText -> do
            target <- textFromCString cTarget
            txt <- textFromCString cText
            modifyIORef queueRef (ProcessingInstruction target txt :)
            return True

        setCommentHandler parser $ \_ cText -> do
            txt <- textFromCString cText
            modifyIORef queueRef (Comment txt :)
            return True

        return (HexpatParser parser queueRef)

data HexpatParseException = HexpatParseExceptio String deriving (Typeable, Show)
instance Exception HexpatParseException

parseBS
  :: (GenericXMLString text, GenericXMLString tag) =>
     ParseOptions tag text
     -> Conduit BS.ByteString IO (SAXEvent tag text)
parseBS opts = conduitIO
                  (createParser opts)
                  (\_ -> return ())
                  (\(HexpatParser parser queueRef) input -> do
                      error <- withParser parser $ \pp -> parseChunk pp input False
                      case error of
                        Nothing -> return ()
                        Just (XMLParseError err _) ->
                          resourceThrow $ HexpatParseExceptio err
                      queue <- readIORef queueRef
                      writeIORef queueRef []
                      return . IOProducing $ reverse queue
                  )
                  (\(HexpatParser parser queueRef) -> do
                      error <- withParser parser $ \pp -> parseChunk pp BS.empty True
                      case error of
                        Nothing -> return ()
                        Just (XMLParseError err _) ->
                          resourceThrow $ HexpatParseExceptio err
                      queue <- readIORef queueRef
                      writeIORef queueRef []
                      return $ reverse queue
                  )

whileJust :: Monad m => m (Maybe a) -> m [a]
whileJust f = do
  f' <- f
  case f' of
    Just x -> liftM (x :) $ whileJust f
    Nothing -> return []



data StreamUnfinishedException = StreamUnfinishedException deriving (Typeable, Show)
instance Exception StreamUnfinishedException


elementFromEvents
  :: (Eq tag, Show tag, MonadIO m, Resource m) =>
     Sink (SAXEvent tag text) m (NodeG [] tag text)
elementFromEvents = do
  Just (StartElement name attrs) <- CL.head
  children <- liftM catMaybes . whileJust $ do
      next' <- CL.peek
      next <- case next' of
        Nothing -> liftIO . throwIO $ StreamUnfinishedException
        Just n -> return n
      case next of
          StartElement _ _  -> Just . Just <$> elementFromEvents
          EndElement n -> if n == name then CL.drop 1 >> return Nothing
                                       else error $ "closing wrong element: "
                                            ++ show n ++ " instead of " ++ show name
          CharacterData txt -> CL.drop 1 >> (return . Just . Just $ Text txt)
          _ -> return $ Just Nothing
  return $ Element name attrs children

openElementFromEvents
  :: Resource m => Sink (SAXEvent tag text) m (NodeG [] tag text)
openElementFromEvents = do
  throwOutJunk
  Just (StartElement name attrs) <- CL.head
  return $ Element name attrs []

throwOutJunk :: Resource m => Sink (SAXEvent t t1) m ()
throwOutJunk = do
  next <- peek
  case next of
    Nothing -> return ()
    Just (StartElement _ _) -> return ()
    _ -> CL.drop 1 >> throwOutJunk

saxToElements
  :: (Eq tag, Show tag, MonadIO m, Resource m) =>
     Conduit (SAXEvent tag text) m (Node tag text)
saxToElements = C.sequence $ throwOutJunk >> elementFromEvents

