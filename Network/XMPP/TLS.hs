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


{-# OPTIONS_HADDOCK hide #-}

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
