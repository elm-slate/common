module Slate.Common.Entity
    exposing
        ( EntityId
        , EntityName
        , PropertyId
        , PropertyName
        , EntityDict
        , PropertyList
        , mtPropertyList
        , propertyListDecoder
        , propertyListEncode
        )

{-|
    Slate Entity module

@docs EntityId , EntityName , PropertyId , PropertyName , EntityDict, PropertyList, mtPropertyList, propertyListDecoder, propertyListEncode
-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Utils.Json as JsonU exposing ((///), (<||))


{-|
    Entity id.
-}
type alias EntityId =
    String


{-|
    Entity name.
-}
type alias EntityName =
    String


{-|
    Property id.
-}
type alias PropertyId =
    String


{-|
    Property name.
-}
type alias PropertyName =
    String


{-|
    Entity Dictionary.
-}
type alias EntityDict entity =
    Dict EntityId entity


{-|
    Property List to be used in Entity Fragments.
-}
type alias PropertyList itemType =
    { ids : List PropertyId
    , items : List itemType
    }


{-|
    Empty PropertyList.
-}
mtPropertyList : PropertyList itemType
mtPropertyList =
    { ids = [], items = [] }


{-|
    PropertyList decoder.
-}
propertyListDecoder : Decoder itemType -> Decoder (PropertyList itemType)
propertyListDecoder itemDecoder =
    succeed PropertyList
        <|| (field "ids" <| JD.list JD.string)
        <|| (field "items" <| JD.list itemDecoder)


{-|
    PropertyList encode.
-}
propertyListEncode : (itemType -> JE.Value) -> (PropertyList itemType -> JE.Value)
propertyListEncode typeEncode =
    JE.list << (List.map typeEncode) << .items
