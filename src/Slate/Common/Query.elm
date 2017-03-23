module Slate.Common.Query
    exposing
        ( ComparableEventType
        , eventTypeToComparable
        , getEntityNameAndEventTypes
        , getPropertyEventTypes
        )

{-|
    Slate Query module.

@docs ComparableEventType, eventTypeToComparable, getEntityNameAndEventTypes, getPropertyEventTypes
-}

import Utils.Ops exposing (..)
import Slate.Common.Entity exposing (..)
import Slate.Common.Event exposing (..)
import Slate.Common.Schema exposing (..)


{-|
    Comparable version of EventType for Dict keys.
-}
type alias ComparableEventType =
    ( Target, Operation, PropertyName )


{-|
    Convert EventType into a ComparableEventType.
-}
eventTypeToComparable : EventType -> ComparableEventType
eventTypeToComparable ( target, operation, maybePropertyName ) =
    ( target, operation, maybePropertyName |?> identity ?= "" )


{-|
    Get the entity name and event types for an entity.
-}
getEntityNameAndEventTypes : EntitySchema -> ( EntityName, List EventType )
getEntityNameAndEventTypes schema =
    ( schema.entityName
    , [ ( "entity", "created", Nothing )
      , ( "entity", "destroyed", Nothing )
      ]
    )


{-|
    Get the event types for a property.
-}
getPropertyEventTypes : PropertySchema -> List EventType
getPropertyEventTypes schema =
    case schema of
        SinglePropertySchema propertyName ->
            [ ( "property", "added", Just propertyName )
            , ( "property", "removed", Just propertyName )
            ]

        MultiplePropertySchema propertyName ->
            [ ( "propertyList", "added", Just propertyName )
            , ( "propertyList", "removed", Just propertyName )
            , ( "propertyList", "positioned", Just propertyName )
            ]

        SingleRelationshipSchema propertyName _ _ ->
            [ ( "relationship", "added", Just propertyName )
            , ( "relationship", "removed", Just propertyName )
            ]

        MultipleRelationshipSchema propertyName _ _ ->
            [ ( "relationshipList", "added", Just propertyName )
            , ( "relationshipList", "removed", Just propertyName )
            , ( "relationshipList", "positioned", Just propertyName )
            ]
