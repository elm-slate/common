module Slate.Common.Schema
    exposing
        ( EntitySchema
        , PropertySchema(..)
        , mtEntitySchema
        , getPropertyName
        , getPropertyEntitySchema
        , isPropertyOwned
        , isPropertyMultiple
        , isPropertyRelationship
        )

{-|
    Slate Schema module.

    Slate entity schemas are defined using this module.

@docs EntitySchema  , PropertySchema , mtEntitySchema, getPropertyName, getPropertyEntitySchema, isPropertyOwned, isPropertyMultiple, isPropertyRelationship
-}

import Slate.Common.Entity exposing (..)


-- API


{-|
    Schema for the entity.
-}
type alias EntitySchema =
    { entityName : EntityName
    , properties : List PropertySchema
    }


{-|
    Schema for entity properties.
-}
type PropertySchema
    = SinglePropertySchema String
    | MultiplePropertySchema String
    | SingleRelationshipSchema String EntitySchema Bool
    | MultipleRelationshipSchema String EntitySchema Bool


{-|
    Null entity schema.
-}
mtEntitySchema : EntitySchema
mtEntitySchema =
    { entityName = ""
    , properties = []
    }


{-|
    Get property name.
-}
getPropertyName : PropertySchema -> String
getPropertyName schema =
    case schema of
        SinglePropertySchema propertyName ->
            propertyName

        MultiplePropertySchema propertyName ->
            propertyName

        SingleRelationshipSchema propertyName _ _ ->
            propertyName

        MultipleRelationshipSchema propertyName _ _ ->
            propertyName


{-|
    Get property entity schema.
-}
getPropertyEntitySchema : PropertySchema -> Maybe EntitySchema
getPropertyEntitySchema schema =
    case schema of
        SinglePropertySchema _ ->
            Nothing

        MultiplePropertySchema _ ->
            Nothing

        SingleRelationshipSchema _ entitySchema _ ->
            Just entitySchema

        MultipleRelationshipSchema _ entitySchema _ ->
            Just entitySchema


{-|
    Check to see if property is owned.
-}
isPropertyOwned : PropertySchema -> Bool
isPropertyOwned schema =
    case schema of
        SinglePropertySchema _ ->
            False

        MultiplePropertySchema _ ->
            False

        SingleRelationshipSchema _ _ owned ->
            owned

        MultipleRelationshipSchema _ _ owned ->
            owned


{-|
    Check to see if property has multiple values.
-}
isPropertyMultiple : PropertySchema -> Bool
isPropertyMultiple schema =
    case schema of
        SinglePropertySchema _ ->
            False

        MultiplePropertySchema _ ->
            True

        SingleRelationshipSchema _ _ _ ->
            False

        MultipleRelationshipSchema _ _ _ ->
            True


{-|
    Check to see if property is a relationship.
-}
isPropertyRelationship : PropertySchema -> Bool
isPropertyRelationship schema =
    case schema of
        SinglePropertySchema _ ->
            False

        MultiplePropertySchema _ ->
            False

        SingleRelationshipSchema _ _ _ ->
            True

        MultipleRelationshipSchema _ _ _ ->
            True
