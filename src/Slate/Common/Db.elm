module Slate.Common.Db
    exposing
        ( DbConnectionInfo
        , makeComparable
        )

{-|
    Slate Entity module

@docs DbConnectionInfo, makeComparable
-}

import Utils.Record as Record


{-|
    Slate DB Connection Information.
-}
type alias DbConnectionInfo =
    { host : String
    , port_ : Int
    , database : String
    , user : String
    , password : String
    , timeout : Int
    }


{-| make connection info comparable for Dictionaries
-}
makeComparable : DbConnectionInfo -> String
makeComparable =
    Record.makeComparable
        [ .host
        , toString << .port_
        , .database
        , .user
        ]
