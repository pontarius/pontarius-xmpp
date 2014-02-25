* 0.3 to 0.4

** Major changes
Added Lenses
Added Plugins

** newly exported functions
simpleAuth
jid (QuasiQuoter)
presenceUnsubscribed
associatedErrorType
mkStanzaError

** major bugs fixed
Didn't check jid of IQResults

** incompatible changes
*** IQ
sendIQ returns an STM action rather than a TMVar
sendIQ' takes a timeout parameter
removed IQResponseTimeout from IQResponse data type
renamed listenIQChan to listenIQ and changed return type from TChan to STM
renamed dropIQChan to unlistenIQ
