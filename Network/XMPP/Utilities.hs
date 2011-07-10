{-

Copyright © 2010-2011 Jon Kristensen.

This file is part of Pontarius XMPP.

Pontarius XMPP is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Pontarius XMPP is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with Pontarius XMPP. If not, see <http://www.gnu.org/licenses/>.

-}

-- |
-- Module:      $Header$
-- Description: Utility functions for Pontarius XMPP; currently only random ID
--              generation functions
-- Copyright:   Copyright © 2010-2011 Jon Kristensen
-- License:     LGPL-3
--
-- Maintainer:  info@pontarius.org
-- Stability:   unstable
-- Portability: portable
--
-- This module will be documented soon.

-- TODO: Document this module
-- TODO: Make is possible to customize characters
-- TODO: Make it possible to customize length

module Network.XMPP.Utilities ( elementToString
                              , elementsToString ) where

import Data.Word
import Data.XML.Types
import System.Crypto.Random
import System.Random
import qualified Data.ByteString as DB
import qualified Data.Map as DM
import qualified Data.Text as DT



-- =============================================================================
--  XML Utilities
-- =============================================================================


elementsToString :: [Element] -> String
elementsToString [] = ""
elementsToString (e:es) = (elementToString $ Just e) ++ elementsToString es

elementToString :: Maybe Element -> String
elementToString Nothing = ""
elementToString (Just e) = "<" ++ nameToString (elementName e) ++ xmlns ++
                           attributes (elementAttributes e) ++
                           ">" ++ (nodesToString $ elementNodes e) ++ "</" ++
                           nameToString (elementName e) ++ ">"
  where
    xmlns :: String
    xmlns = case nameNamespace $ elementName e of
      Nothing -> ""
      Just t -> " xmlns='" ++ (DT.unpack t) ++ "'"

    nameToString :: Name -> String
    nameToString Name { nameLocalName = n, namePrefix = Nothing } = DT.unpack n
    nameToString Name { nameLocalName = n, namePrefix = Just p } =
      (DT.unpack p) ++ ":" ++ (DT.unpack n)

    contentToString :: Content -> String
    contentToString (ContentText t) = DT.unpack t
    contentToString (ContentEntity t) = DT.unpack t

    attributes :: [(Name, [Content])] -> String
    attributes [] = ""
    attributes ((n, c):t) = (" " ++ (nameToString n) ++ "='" ++
                             concat (map contentToString c) ++ "'") ++
                            attributes t

    nodesToString :: [Node] -> String
    nodesToString [] = ""
    nodesToString ((NodeElement e):ns) = (elementToString $ Just e) ++
                                         (nodesToString ns)
    nodesToString ((NodeContent c):ns) = (contentToString c) ++
                                         (nodesToString ns)
