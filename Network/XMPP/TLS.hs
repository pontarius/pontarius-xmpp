-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

module Network.XMPP.TLS (tlsParams) where

import Network.TLS
import Network.TLS.Extra -- (cipher_AES128_SHA1)
import Network.TLS.Cipher
import Crypto.Hash.SHA1
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)
import Data.Time.Calendar

tlsParams :: TLSParams

tlsParams = TLSParams { pConnectVersion    = TLS10 -- TODO: TLS12 when supported in tls; TODO: TLS11 results in a read error - bug?
                      , pAllowedVersions   = [SSL3, TLS10,TLS11] -- TODO: TLS12 when supported in tls
                      , pCiphers           = [cipher_AES128_SHA1] -- TODO: cipher_AES128_SHA1 = TLS_RSA_WITH_AES_128_CBC_SHA?
                      , pCompressions      = [nullCompression] -- TODO
                      , pWantClientCert    = False -- Used for servers
                      , pUseSecureRenegotiation = False -- TODO: No renegotiation!
                      , pCertificates      = [] -- TODO
                      , pLogging           = defaultLogging -- TODO
                      , onCertificatesRecv = \ certificate -> do
    putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    putStrLn $ show certificate
    putStrLn "0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! verify chain (will be false if self-signed - not the case)"
    lolz <- certificateVerifyChain certificate
    putStrLn $ show lolz
    putStrLn "1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! self signed (only cas can be self-signed)"
    putStrLn $ show $ certificateSelfSigned $ head certificate
    putStrLn "2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! verify domain"
    putStrLn $ show $ certificateVerifyDomain "jonkristensen.com" certificate
    putStrLn "3 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! verify validity"
    putStrLn $ show $ certificateVerifyValidity (fromGregorian 2011 07 14) certificate
    putStrLn "4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!! fingerprint (didn't change when i changed last bytes - good!)"
    putStrLn $ show $ certificateFingerprint hashlazy $ head certificate
    putStrLn "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    return CertificateUsageAccept } -- TODO


