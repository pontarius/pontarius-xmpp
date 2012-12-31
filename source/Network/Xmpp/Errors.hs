{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Errors where

import           Control.Applicative ((<$>))
import           Control.Monad(unless)
import           Control.Monad.Error
import           Control.Monad.Error.Class
import qualified Data.Text as Text
import           Data.XML.Types
import           Network.Xmpp.Types
import           Network.Xmpp.Pickle


