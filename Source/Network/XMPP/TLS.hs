-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

-- TODO: TLS12 when supported in tls; TODO: TLS11 results in a read error - bug?
-- TODO: cipher_AES128_SHA1 = TLS_RSA_WITH_AES_128_CBC_SHA?
-- TODO: Compression?
-- TODO: Validate certificate

{-# OPTIONS_HADDOCK hide #-}

module Network.XMPP.TLS (tlsParams) where

import Network.TLS (TLSCertificateUsage (CertificateUsageAccept),
                    TLSParams (..), Version (SSL3, TLS10, TLS11),
                    defaultLogging, nullCompression)
import Network.TLS.Extra (cipher_AES128_SHA1)


tlsParams :: TLSParams

tlsParams = TLSParams { pConnectVersion    = TLS10
                      , pAllowedVersions   = [SSL3, TLS10,TLS11]
                      , pCiphers           = [cipher_AES128_SHA1]
                      , pCompressions      = [nullCompression]
                      , pWantClientCert    = False -- Used for servers
                      , pUseSecureRenegotiation = False -- No renegotiation
                      , pCertificates      = [] -- TODO
                      , pLogging           = defaultLogging -- TODO
                      , onCertificatesRecv = \ certificate ->
                                             return CertificateUsageAccept }
