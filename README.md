Pontarius XMPP
==============

Pontarius XMPP is a Haskell XMPP client library that implements the capabilities
of [RFC 6120 ("XMPP CORE")](http://tools.ietf.org/html/rfc6120), [RFC 6121
("XMPP IM")](http://tools.ietf.org/html/rfc6121), and [RFC 6122 ("XMPP
ADDR")](http://tools.ietf.org/html/rfc6122). Pontarius XMPP was part of the
Pontarius project, an effort to produce free and open source, uncentralized, and
privacy-aware software solutions.

While in alpha, Pontarius XMPP works quite well and fulfills most requirements
of the RFCs.

Prerequisites
-------------

Pontarius XMPP requires GHC 7.0, or later.

You will need the ICU Unicode library and it's header files in order to be able
to build Pontarius XMPP. On Debian, you will need to install the *libicu-dev*
package. In Fedora, the package is called *libicu-devel*.

_Note to users of GHC 7.0 and GHC 7.2:_ You will need *cabal-install*, version
*0.14.0* or higher, or the build will fail with an "unrecognized option:
--disable-benchmarks" error. The versions *1.16.0* and higher might not build on
your system; if so, install *0.14.0* with "cabal install cabal-install-0.14.0".

_Note to users of GHC 7.2.1:_ Due to a bug, recent versions of the *binary*
package wont build without running "ghc-pkg trust base".

_Note to users of GHC 7.0.1:_ You will want to configure your Cabal environment
(including *cabal-install*) for version *0.9.2.1* of *bytestring*.

Getting started
---------------

The latest release of Pontarius XMPP, as well as its module API pages, can
always be found at [the Pontarius XMPP Hackage
page](http://hackage.haskell.org/package/pontarius-xmpp/).

_Note:_ Pontarius XMPP is still in its Alpha phase. Pontarius XMPP is not yet
feature-complete, it may contain bugs, and its API may change between versions.

The first thing to do is to import the modules that we are going to use. We are
also using the OverloadedStrings LANGUAGE pragma in order to be able to type
<code>Text</code> values like strings.

    {-# LANGUAGE OverloadedStrings #-}

    import Network.Xmpp

    import Control.Monad
    import Data.Default
    import System.Log.Logger

Pontarius XMPP supports [hslogger](http://hackage.haskell.org/package/hslogger)
logging. Start by enabling console logging.

    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

When this is done, a <code>Session</code> object can be acquired by calling
<code>session</code>. This object will be used for interacting with the library.

    result <- session
                 "example.com"
                  (Just (\_ -> ( [scramSha1 "username" Nothing "password"])
                               , Nothing))
                  def

The three parameters above are the XMPP server realm, a SASL handler (for
authentication), and the session configuration settings (set to the default
settings). <code>session</code> will perform the necessary DNS queries to find
the address of the realm, connect, establish the XMPP stream, attempt to secure
the stream with TLS, authenticate, establish a concurrent interface for
interacting with the stream, and return the <code>Session</code> object.

The return type of <code>session</code> is <code>IO (Either XmppFailure
Session)</code>. As <code>XmppFailure</code> is an
<code>Control.Monad.Error</code> instance, you can utilize the
<code>ErrorT</code> monad transformer for error handling. A more simple way of
doing it could be doing something like this:

    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ (show e)

Next, let us set our status to Online.

    sendPresence def sess

Here, <code>def</code> refers to the default <code>Presence</code> value, which
is the same as <code>Presence Nothing Nothing Nothing Nothing Available
[]</code>.

Now, let's say that we want to receive all message stanzas, and echo the stanzas
back to the sender. This can be done like so:

    forever $ do
        msg <- getMessage sess
        case answerMessage msg (messagePayload msg) of
            Just answer -> sendMessage answer sess
            Nothing -> putStrLn "Received message with no sender."

You don't need to worry about escaping your <code>Text</code> values - Pontarius
XMPP (or rather, [xml-picklers](https://github.com/Philonous/xml-picklers)) will
take care of that for you.

Additional XMPP threads can be created using <code>dupSession</code> and
<code>forkIO</code>.

For a public domain example of a simple Pontarius XMPP (Cabal) project, refer to
the examples/echoclient directory.
