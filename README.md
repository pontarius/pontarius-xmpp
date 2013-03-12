Welcome to Pontarius XMPP!
==========================

Pontarius XMPP is an active work in progress to build a Haskell XMPP library
that implements the client capabilities of [RFC 6120 ("XMPP
Core")](http://tools.ietf.org/html/rfc6120).

Getting started
---------------

The latest release of Pontarius XMPP, as well as its module API pages, can
always be found at [the Pontarius XMPP Hackage
page](http://hackage.haskell.org/package/pontarius-xmpp/).

_Note:_ Pontarius XMPP is still in its Alpha phase. Pontarius XMPP is not yet
feature-complete, it may contain bugs, and its API may change between versions.

The first thing to do is to import the modules that we are going to use.

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
                  def
                  (Just ([scramSha1 "user" Nothing "Password"], Nothing))

_Tip:_ Note that the first parameter actually is a Text value. Import
<code>Data.Text</code> and use the OverloadedStrings LANGUAGE pragma.

The three parameters above are the XMPP server realm, the session configuration
settings (set to the default settings), and a SASL handler (for authentication).
<code>session</code> will perform the necessary DNS queries to find the address
of the realm, connect, establish the XMPP stream, attempt to secure the stream
with TLS, authenticate, establish a concurrent interface for interacting with
the stream, and return the <code>Session</code> object.

The return type of <code>session</code> is <code>IO (Either XmppFailure
(Session, Maybe AuthFailure))</code>. As <code>XmppFailure</code> is an
<code>Control.Monad.Error</code> instance, you can utilize the
<code>ErrorT</code> monad transformer for error handling. A more simple way of
doing it could be doing something like this:

    sess <- case result of
                Right (s, Nothing) -> return s
                Right (_s, e) -> error $ "AuthFailure: " ++ (show e)
                Left e -> error $ "XmppFailure: " ++ (show e)

Next, let us set our status to Online.

    sendPresence (Presence Nothing Nothing Nothing Nothing Nothing []) sess

Now, let's say that we want to receive all message stanzas, and echo the stanzas
back to the recipient. This can be done like so:

    forever $ do
        msg <- getMessage sess
        case answerMessage msg (messagePayload msg) of
            Just answer -> sendMessage answer sess
            Nothing -> putStrLn "Received message with no sender."

Additional XMPP threads can be created using <code>dupSession</code> and
<code>forkIO</code>.

For a public domain example of a simple Pontarius XMPP (Cabal) project, refer to
the examples/echoclient directory.

More information
----------------

Feel free to [contact Jon Kristensen](http://www.jonkri.com/contact/) if you
have any questions or comments.
