-----------------------------------------------------------------------------
--
-- Module      :  Network.XMPP.TLS
-- Copyright   :  Copyright © 2011, Jon Kristensen
-- License     :  LGPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  jon.kristensen@pontarius.org
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Network.XMPP.TLS (
getTLSParams,
handshake'
) where

import Network.TLS
import Network.TLS.Cipher
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)


getTLSParams :: TLSParams
getTLSParams = TLSParams { pConnectVersion    = TLS10
                    , pAllowedVersions   = [TLS10,TLS11]
                    , pCiphers           = [cipher_AES256_SHA1] -- Check the rest
                    , pCompressions      = [nullCompression]
                    , pWantClientCert    = False
                    , pCertificates      = []
                    , onCertificatesRecv = \_ -> return True } -- Verify cert chain

handshake' :: Handle -> String -> IO (Maybe TLSCtx)
handshake' h s = do
  let t = getTLSParams
  r <- makeSRandomGen
  case r of
    Right sr -> do
      putStrLn $ show sr
      c <- client t sr h
      handshake c
      putStrLn ">>>>TLS data sended<<<<"
      return (Just c)
    Left ge -> do
      putStrLn $ show ge
      return Nothing
