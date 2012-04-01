{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

module Data.Conduit.Hexpat
       ( ParseOptions(..)
       , defaultParseOptions
       , parseBS
       )

       where

import           Control.Applicative((<$>))
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class

import qualified Data.ByteString as BS
import           Data.Conduit as C
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import           Data.Text(Text)
import           Data.Typeable
import           Data.XML.Types as XML

import           Text.XML.Expat.Internal.IO hiding (parse)

import           Data.IORef
-- adapted from parseG

-- | Parse a generalized list of ByteStrings containing XML to SAX events.
-- In the event of an error, FailDocument is the last element of the output list.

data HexpatParser = HexpatParser
                    { hParser :: Parser
                    , hQueueRef :: IORef [XML.Event]
                    }

splitName :: Text -> Name
splitName name = case Text.split (=='}') name of
                         [n] -> case Text.split (==':') n of
                           [n'] -> Name n' Nothing Nothing
                           [p,n'] -> Name n' Nothing (Just p)
                           _ -> throw . HexpatParseException
                                $ "Error parsing name: " ++ show name
                         [ns,n] -> Name n (Just ns) Nothing
                         _ -> throw . HexpatParseException
                              $ "Error parsing name: " ++ show name

createParser ::  ParseOptions -> Maybe Char -> IO (HexpatParser)
createParser opts delim =  do
        let enc = overrideEncoding opts
--        let mEntityDecoder = entityDecoder opts
        parser <- newParser enc delim
        queueRef <- newIORef []

        -- setXMLDeclarationHandler parser $ \_ cVer cEnc cSd -> do
        --     ver <- textFromCString cVer
        --     mEnc <- if cEnc == nullPtr
        --         then return Nothing
        --         else Just <$> textFromCString cEnc
        --     let sd = if cSd < 0
        --             then Nothing
        --             else Just $ if cSd /= 0 then True else False
        --     modifyIORef queueRef (XMLDeclaration ver mEnc sd:)
        --     TODO: What to do here?
        --     return True

        setStartElementHandler parser $ \_ cName cAttrs -> do
            name <- splitName <$> textFromCString cName
            attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
                attrName <- splitName <$> textFromCString cAttrName
                attrValue <- ContentText <$> textFromCString cAttrValue
                return (attrName, [attrValue])
            modifyIORef queueRef (EventBeginElement name attrs:)
            return True

        setEndElementHandler parser $ \_ cName -> do
            name <- splitName <$> textFromCString cName
            modifyIORef queueRef (EventEndElement name:)
            return True

        setCharacterDataHandler parser $ \_ cText -> do
            txt <- TE.decodeUtf8 <$> BS.packCStringLen cText
            modifyIORef queueRef ((EventContent $ ContentText txt):)
            return True

        setProcessingInstructionHandler parser $ \_ cTarget cText -> do
            target <- textFromCString cTarget
            txt <- textFromCString cText
            modifyIORef queueRef (EventInstruction (Instruction target txt) :)
            return True

        setCommentHandler parser $ \_ cText -> do
            txt <- textFromCString cText
            modifyIORef queueRef (EventComment txt :)
            return True

        return (HexpatParser parser queueRef)

data HexpatParseException = HexpatParseException String deriving (Typeable, Show)
instance Exception HexpatParseException

parseBS
  :: (MonadResource (t IO), MonadTrans t) =>
     ParseOptions -> Conduit BS.ByteString (t IO) Event
parseBS opts = conduitIO
                  (createParser opts (Just '}'))
                  (\_ -> return ())
                  (\(HexpatParser parser queueRef) input -> lift $ do
                      e <- withParser parser $ \pp -> parseChunk pp input False
                      case e of
                        Nothing -> return ()
                        Just (XMLParseError err _) ->
                          throwIO $ HexpatParseException err
                      queue <- readIORef queueRef
                      writeIORef queueRef []
                      return . IOProducing $ reverse queue
                  )
                  (\(HexpatParser parser queueRef) -> lift $ do
                      e <- withParser parser $ \pp -> parseChunk pp BS.empty True
                      case e of
                        Nothing -> return ()
                        Just (XMLParseError err _) ->
                          throwIO $ HexpatParseException err
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


