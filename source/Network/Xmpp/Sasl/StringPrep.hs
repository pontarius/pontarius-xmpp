module Network.Xmpp.Sasl.StringPrep where

import Text.StringPrep

saslPrepQuery = Profile
    [b1]
    True
    [ c12
    , c21
    , c22
    , c3
    , c4
    , c5
    , c6
    , c7
    , c8
    , c9
    ]
    True

saslPrepStore = Profile
    [b1]
    True
    [ a1
    , c12
    , c21
    , c22
    , c3
    , c4
    , c5
    , c6
    , c7
    , c8
    , c9
    ]
    True

normalizePassword = runStringPrep saslPrepStore
normalizeUsername = runStringPrep saslPrepQuery