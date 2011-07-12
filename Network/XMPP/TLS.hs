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

module Network.XMPP.TLS (tlsParams) where

import Network.TLS
import Network.TLS.Extra (cipher_AES128_SHA1)
import Network.TLS.Cipher
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)

tlsParams :: TLSParams
tlsParams = TLSParams { pConnectVersion    = TLS10 -- TODO: TLS12 when supported in tls; TODO: TLS11 results in a read error - bug?
                      , pAllowedVersions   = [SSL3, TLS10,TLS11] -- TODO: TLS12 when supported in tls
                      , pCiphers           = [cipher_AES128_SHA1] -- TODO: cipher_AES128_SHA1 = TLS_RSA_WITH_AES_128_CBC_SHA?
                      , pCompressions      = [nullCompression] -- TODO
                      , pWantClientCert    = False -- Used for servers
                      , pUseSecureRenegotiation = False -- TODO: No renegotiation!
                      , pCertificates      = [] -- TODO
                      , pLogging           = defaultLogging -- TODO
                      , onCertificatesRecv = \_ -> return CertificateUsageAccept } -- TODO
