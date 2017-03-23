module Slate.Common.Relationship
    exposing
        ( entityRelationshipEncode
        , entityRelationshipDecoder
        )

{-|
    Slate Relationship module.

@docs  entityRelationshipEncode , entityRelationshipDecoder
-}

import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing (..)
import Slate.Common.Entity exposing (..)


{-|
    EntityId Json encoder.
-}
entityRelationshipEncode : EntityId -> JE.Value
entityRelationshipEncode =
    JE.string


{-|
    EntityId Json decoder.
-}
entityRelationshipDecoder : Decoder EntityId
entityRelationshipDecoder =
    JD.string
