module Slate.Common.Db exposing (DbConnectionInfo)

{-|
    Slate Entity module

@docs DbConnectionInfo
-}


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
