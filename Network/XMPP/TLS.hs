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

import Crypto.Random (newGenIO, SystemRandom)
import Network.TLS
import Network.TLS.Extra (cipher_AES128_SHA1)
import Network.TLS.Cipher
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)


getTLSParams :: TLSParams
getTLSParams = TLSParams { pConnectVersion    = TLS10
                         , pAllowedVersions   = [TLS10,TLS11]
                         , pCiphers           = [cipher_AES128_SHA1] -- Check the rest
                         , pCompressions      = [nullCompression]
                         , pWantClientCert    = False
                         , pUseSecureRenegotiation = False -- TODO: No renegotiation
                         , pCertificates      = []
                         , pLogging           = defaultLogging
                         , onCertificatesRecv = \_ -> return CertificateUsageAccept } -- Verify cert chain

handshake' :: Handle -> String -> IO (Maybe TLSCtx)
handshake' h s = do
  let t = getTLSParams
  r <- newGenIO :: IO SystemRandom -- Investigate limitations
  c <- client t r h
  handshake c
  putStrLn ">>>>TLS data sended<<<<"
  return (Just c)
