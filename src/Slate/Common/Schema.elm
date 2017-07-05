module Slate.Common.Schema
    exposing
        ( EntitySchema
        , PropertySchema(..)
        , ValueValidator
        , ChangeValidator
        , partialSchemaEncoder
        , noValidationSingleProperty
        , valueValidationOnlySingleProperty
        , changeValidationOnlySingleProperty
        , mtEntitySchema
        , getPropertySchema
        , getPropertyName
        , getPropertyEntitySchema
        , getValueValidator
        , getChangeValidator
        , isPropertyOwned
        , isPropertyMultiple
        , isPropertyRelationship
        )

{-|
    Slate Schema module.

    Slate entity schemas are defined using this module.

@docs EntitySchema  , PropertySchema , ValueValidator, ChangeValidator, partialSchemaEncoder, noValidationSingleProperty, valueValidationOnlySingleProperty , changeValidationOnlySingleProperty, mtEntitySchema, getPropertySchema, getPropertyName, getPropertyEntitySchema, getValueValidator , getChangeValidator,isPropertyOwned, isPropertyMultiple, isPropertyRelationship
-}

import Json.Encode as JE
import Slate.Common.Entity exposing (..)
import Slate.Common.Event exposing (..)


-- API


{-|
    Schema for the entity.
-}
type alias EntitySchema =
    { entityName : EntityName
    , properties : List PropertySchema
    }


{-| Validate function for a single value
-}
type alias ValueValidator =
    Value -> Result String ()


{-| Validate function for a change in value
-}
type alias ChangeValidator =
    Maybe Value -> Value -> Result String ()


type alias SinglePropertyValidator =
    { valueValidator : Maybe ValueValidator
    , changeValidator : Maybe ChangeValidator
    }


{-| Helper structure for Single Properties with no validation
-}
noValidationSingleProperty : SinglePropertyValidator
noValidationSingleProperty =
    SinglePropertyValidator Nothing Nothing


{-| Helper for ONLY Value Validation for Single Properties
-}
valueValidationOnlySingleProperty : ValueValidator -> SinglePropertyValidator
valueValidationOnlySingleProperty valueValidator =
    SinglePropertyValidator (Just valueValidator) Nothing


{-| Helper for ONLY Change Validation for Single Properties
-}
changeValidationOnlySingleProperty : ChangeValidator -> SinglePropertyValidator
changeValidationOnlySingleProperty changeValidator =
    SinglePropertyValidator Nothing (Just changeValidator)


type alias MultiplePropertyValidator =
    { valueValidator : Maybe ValueValidator
    }


{-|
    Schema for entity properties.
-}
type PropertySchema
    = SinglePropertySchema PropertyName SinglePropertyValidator
    | MultiplePropertySchema PropertyName MultiplePropertyValidator
    | SingleRelationshipSchema PropertyName EntitySchema Bool
    | MultipleRelationshipSchema PropertyName EntitySchema Bool


{-|
    Null entity schema.
-}
mtEntitySchema : EntitySchema
mtEntitySchema =
    { entityName = ""
    , properties = []
    }


{-| Get specified property schema.
-}
getPropertySchema : EntitySchema -> PropertyName -> Maybe PropertySchema
getPropertySchema entitySchema propertyName =
    entitySchema.properties
        |> List.filter (\schema -> getPropertyName schema == propertyName)
        |> List.head


{-|
    Get property name.
-}
getPropertyName : PropertySchema -> String
getPropertyName schema =
    case schema of
        SinglePropertySchema propertyName _ ->
            propertyName

        MultiplePropertySchema propertyName _ ->
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
        SinglePropertySchema _ _ ->
            Nothing

        MultiplePropertySchema _ _ ->
            Nothing

        SingleRelationshipSchema _ entitySchema _ ->
            Just entitySchema

        MultipleRelationshipSchema _ entitySchema _ ->
            Just entitySchema


{-| get value validator for property
-}
getValueValidator : PropertySchema -> Maybe ValueValidator
getValueValidator schema =
    case schema of
        SinglePropertySchema _ validator ->
            validator.valueValidator

        MultiplePropertySchema _ validator ->
            validator.valueValidator

        SingleRelationshipSchema _ entitySchema _ ->
            Nothing

        MultipleRelationshipSchema _ entitySchema _ ->
            Nothing


{-| get change validator for property
-}
getChangeValidator : PropertySchema -> Maybe ChangeValidator
getChangeValidator schema =
    case schema of
        SinglePropertySchema _ validator ->
            validator.changeValidator

        MultiplePropertySchema _ _ ->
            Nothing

        SingleRelationshipSchema _ entitySchema _ ->
            Nothing

        MultipleRelationshipSchema _ entitySchema _ ->
            Nothing


{-|
    Check to see if property is owned.
-}
isPropertyOwned : PropertySchema -> Bool
isPropertyOwned schema =
    case schema of
        SinglePropertySchema _ _ ->
            False

        MultiplePropertySchema _ _ ->
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
        SinglePropertySchema _ _ ->
            False

        MultiplePropertySchema _ _ ->
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
        SinglePropertySchema _ _ ->
            False

        MultiplePropertySchema _ _ ->
            False

        SingleRelationshipSchema _ _ _ ->
            True

        MultipleRelationshipSchema _ _ _ ->
            True


{-| partial schema encoder
-}
partialSchemaEncoder : EntitySchema -> JE.Value
partialSchemaEncoder schema =
    JE.object <|
        [ ( "entityName", JE.string schema.entityName )
        , ( "properties", (JE.list << List.map partialPropertySchemaEncoder) schema.properties )
        ]



-- PRIVATE API


partialPropertySchemaEncoder : PropertySchema -> JE.Value
partialPropertySchemaEncoder propertySchema =
    JE.object <|
        case propertySchema of
            SinglePropertySchema propertyName _ ->
                [ ( "type", JE.string "SinglePropertySchema" )
                , ( "propertyName", JE.string propertyName )
                ]

            MultiplePropertySchema propertyName _ ->
                [ ( "type", JE.string "MultiplePropertySchema" )
                , ( "propertyName", JE.string propertyName )
                ]

            SingleRelationshipSchema propertyName schema owned ->
                [ ( "type", JE.string "SingleRelationshipSchema" )
                , ( "propertyName", JE.string propertyName )
                , ( "schemaName", JE.string schema.entityName )
                , ( "owned", JE.bool owned )
                ]

            MultipleRelationshipSchema propertyName schema owned ->
                [ ( "type", JE.string "MultipleRelationshipSchema" )
                , ( "propertyName", JE.string propertyName )
                , ( "schemaName", JE.string schema.entityName )
                , ( "owned", JE.bool owned )
                ]
