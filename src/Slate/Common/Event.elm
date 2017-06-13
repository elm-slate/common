module Slate.Common.Event
    exposing
        ( RelationshipId
        , Value
        , OldPosition
        , NewPosition
        , Position
        , Target
        , Operation
        , EventRecord
        , Event(..)
        , MutatingEvent(..)
        , NonMutatingEvent(..)
        , EventType
        , EntityEventTypes
        , InitiatorId
        , Metadata
        , toBool
        , fromBool
        , eventRecordDecoder
        , eventDecoder
        , encodeEvent
        , getConvertedValue
        , getIntValue
        , getFloatValue
        , getDateValue
        , getBoolValue
        , getStringValue
        , getEntityName
        , getEntityId
        , getPropertyName
        , getTarget
        , getOperation
        , getRelationshipId
        , getPosition
        , getPropertyId
        , getInitiatorId
        , getCommand
        )

{-|
    Slate Event module

@docs RelationshipId , Value , OldPosition , NewPosition , Position, Target, Operation, EventRecord , Event, MutatingEvent, NonMutatingEvent, EventType, EntityEventTypes, InitiatorId, Metadata, toBool, fromBool, eventRecordDecoder, eventDecoder, encodeEvent, getConvertedValue, getIntValue, getFloatValue, getDateValue, getBoolValue, getStringValue, getEntityName, getEntityId, getPropertyName, getTarget, getOperation, getRelationshipId, getPosition, getPropertyId, getInitiatorId, getCommand
-}

import Tuple exposing (..)
import Date exposing (Date)
import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Extra as JDE exposing (..)
import Utils.Ops exposing (..)
import Utils.Json as JsonU exposing (..)
import StringUtils exposing (..)
import Slate.Common.Entity exposing (..)


--  API


{-|
-}
type alias RelationshipId =
    EntityId


{-|
    Property value (primitive).
-}
type alias Value =
    String


{-|
    Old position of property in a list.
-}
type alias OldPosition =
    Int


{-|
    New position of property in a list.
-}
type alias NewPosition =
    Int


{-|
    Position change in a properly list.
-}
type alias Position =
    ( OldPosition, NewPosition )


{-|
    Event Target.
-}
type alias Target =
    String


{-|
    Event Operation.
-}
type alias Operation =
    String


{-|
    Slate Event DB record.
-}
type alias EventRecord =
    { id : String
    , ts : Date
    , event : Event
    , max : Maybe String
    }


{-|
    Slate event.
-}
type Event
    = Mutating MutatingEvent Metadata
    | NonMutating Value Metadata


{-|
    Mutating event.
-}
type MutatingEvent
    = CreateEntity EntityName EntityId
    | DestroyEntity EntityName EntityId
    | AddProperty EntityName EntityId PropertyName Value
    | RemoveProperty EntityName EntityId PropertyName
    | AddPropertyList EntityName EntityId PropertyName PropertyId Value
    | RemovePropertyList EntityName EntityId PropertyName PropertyId
    | PositionPropertyList EntityName EntityId PropertyName Position
    | AddRelationship EntityName EntityId PropertyName EntityId
    | RemoveRelationship EntityName EntityId PropertyName
    | AddRelationshipList EntityName EntityId PropertyName PropertyId EntityId
    | RemoveRelationshipList EntityName EntityId PropertyName PropertyId
    | PositionRelationshipList EntityName EntityId PropertyName Position


{-|
    Non-mutating event.
-}
type NonMutatingEvent
    = NonMutatingEvent Value


{-|
    Type of event.
-}
type alias EventType =
    ( Target, Operation, Maybe PropertyName )


{-| Event type for specified entity.
-}
type alias EntityEventTypes =
    ( EntityName, List EventType )


{-|
    Initiator id.
-}
type alias InitiatorId =
    String


{-|
    Event metadata.
-}
type alias Metadata =
    { initiatorId : InitiatorId
    , command : String
    }


{-| Convert value to Bool since all values are Strings.
-}
toBool : Value -> Bool
toBool strBool =
    case String.toLower strBool of
        "true" ->
            True

        _ ->
            False


{-| Convert Bool to value since all values are Strings.
-}
fromBool : Bool -> Value
fromBool value =
    value ? ( "true", "false" )


{-|
    Event Record decoder.
-}
eventRecordDecoder : JD.Decoder EventRecord
eventRecordDecoder =
    JD.succeed EventRecord
        <|| (JD.field "id" JD.string)
        <|| (JD.field "ts" JDE.date)
        <|| (JD.field "event" eventDecoder)
        <|| (JD.maybe (JD.field "max" JD.string))


{-|
    Event encoder.
-}
encodeEvent : Event -> String
encodeEvent event =
    let
        crash error =
            Debug.crash ("Program bug:" +-+ error)
    in
        JE.encode 0 <|
            JE.object <|
                case event of
                    Mutating mutatingEventData metadata ->
                        List.append
                            [ ( "metadata", metadataEncoder metadata )
                            , ( "target", JE.string <| getTarget event ??= crash )
                            , ( "operation", JE.string <| getOperation event ??= crash )
                            ]
                            (case mutatingEventData of
                                CreateEntity entityName entityId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    ]

                                DestroyEntity entityName entityId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    ]

                                AddProperty entityName entityId propertyName value ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "value", JE.string value )
                                    ]

                                RemoveProperty entityName entityId propertyName ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    ]

                                AddPropertyList entityName entityId propertyName propertyId value ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "propertyId", JE.string propertyId )
                                    , ( "value", JE.string value )
                                    ]

                                RemovePropertyList entityName entityId propertyName propertyId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "propertyId", JE.string propertyId )
                                    ]

                                PositionPropertyList entityName entityId propertyName position ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "oldPosition", JE.int <| first position )
                                    , ( "newPosition", JE.int <| second position )
                                    ]

                                AddRelationship entityName entityId propertyName relationshipId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "relationshipId", JE.string relationshipId )
                                    ]

                                RemoveRelationship entityName entityId propertyName ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    ]

                                AddRelationshipList entityName entityId propertyName propertyId relationshipId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "propertyId", JE.string propertyId )
                                    , ( "relationshipId", JE.string relationshipId )
                                    ]

                                RemoveRelationshipList entityName entityId propertyName propertyId ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "propertyId", JE.string propertyId )
                                    ]

                                PositionRelationshipList entityName entityId propertyName position ->
                                    [ ( "entityName", JE.string entityName )
                                    , ( "entityId", JE.string entityId )
                                    , ( "propertyName", JE.string propertyName )
                                    , ( "oldPosition", JE.int <| first position )
                                    , ( "newPosition", JE.int <| second position )
                                    ]
                            )

                    NonMutating value metadata ->
                        [ ( "metadata", metadataEncoder metadata )
                        , ( "target", JE.string "none" )
                        , ( "value", JE.string value )
                        ]


{-|
    Convert value from event with specified function.
-}
getConvertedValue : (String -> Result String value) -> Event -> Result String value
getConvertedValue convert event =
    let
        errorResult =
            Err ("Event:" +-+ event +-+ "doesn't have a value")
    in
        case event of
            Mutating mutatingEventData _ ->
                case mutatingEventData of
                    AddProperty _ _ _ value ->
                        convert value

                    AddPropertyList _ _ _ _ value ->
                        convert value

                    _ ->
                        errorResult

            NonMutating _ _ ->
                Err "Non-mutating events not supported"


{-|
   Get Int value from event.
-}
getIntValue : Event -> Result String Int
getIntValue event =
    getConvertedValue String.toInt event


{-|
    Get Float value from event.
-}
getFloatValue : Event -> Result String Float
getFloatValue event =
    getConvertedValue String.toFloat event


{-|
    Get Date value from event.
-}
getDateValue : Event -> Result String Date
getDateValue event =
    getConvertedValue Date.fromString event


{-|
    Get Bool value from event.
-}
getBoolValue : Event -> Result String Bool
getBoolValue event =
    getConvertedValue (Ok << toBool) event


{-|
    Get String value from event.
-}
getStringValue : Event -> Result String String
getStringValue event =
    getConvertedValue (Ok << identity) event


{-|
    Get EntityName from event.
-}
getEntityName : Event -> Result String EntityName
getEntityName event =
    case event of
        Mutating mutatingEventData _ ->
            case mutatingEventData of
                CreateEntity entityName _ ->
                    Ok <| entityName

                DestroyEntity entityName _ ->
                    Ok <| entityName

                AddProperty entityName _ _ _ ->
                    Ok <| entityName

                RemoveProperty entityName _ _ ->
                    Ok <| entityName

                AddPropertyList entityName _ _ _ _ ->
                    Ok <| entityName

                RemovePropertyList entityName _ _ _ ->
                    Ok <| entityName

                PositionPropertyList entityName _ _ _ ->
                    Ok <| entityName

                AddRelationship entityName _ _ _ ->
                    Ok <| entityName

                RemoveRelationship entityName _ _ ->
                    Ok <| entityName

                AddRelationshipList entityName _ _ _ _ ->
                    Ok <| entityName

                RemoveRelationshipList entityName _ _ _ ->
                    Ok <| entityName

                PositionRelationshipList entityName _ _ _ ->
                    Ok <| entityName

        NonMutating _ _ ->
            Err "Non-mutating events not supported"


{-|
    Get EntityId from event.
-}
getEntityId : Event -> Result String EntityId
getEntityId event =
    case event of
        Mutating mutatingEventData _ ->
            case mutatingEventData of
                CreateEntity _ entityId ->
                    Ok <| entityId

                DestroyEntity _ entityId ->
                    Ok <| entityId

                AddProperty _ entityId _ _ ->
                    Ok <| entityId

                RemoveProperty _ entityId _ ->
                    Ok <| entityId

                AddPropertyList _ entityId _ _ _ ->
                    Ok <| entityId

                RemovePropertyList _ entityId _ _ ->
                    Ok <| entityId

                PositionPropertyList _ entityId _ _ ->
                    Ok <| entityId

                AddRelationship _ entityId _ _ ->
                    Ok <| entityId

                RemoveRelationship _ entityId _ ->
                    Ok <| entityId

                AddRelationshipList _ entityId _ _ _ ->
                    Ok <| entityId

                RemoveRelationshipList _ entityId _ _ ->
                    Ok <| entityId

                PositionRelationshipList _ entityId _ _ ->
                    Ok <| entityId

        NonMutating _ _ ->
            Err "Non-mutating events not supported"


{-|
    Get Property name from event.
-}
getPropertyName : Event -> Result String PropertyName
getPropertyName event =
    let
        errorResult =
            Err ("Event:" +-+ event +-+ "doesn't have a propertyName")
    in
        case event of
            Mutating mutatingEventData _ ->
                case mutatingEventData of
                    AddProperty _ _ propertyName _ ->
                        Ok <| propertyName

                    RemoveProperty _ _ propertyName ->
                        Ok <| propertyName

                    AddPropertyList _ _ propertyName _ _ ->
                        Ok <| propertyName

                    RemovePropertyList _ _ propertyName _ ->
                        Ok <| propertyName

                    PositionPropertyList _ _ propertyName _ ->
                        Ok <| propertyName

                    AddRelationship _ _ propertyName _ ->
                        Ok <| propertyName

                    RemoveRelationship _ _ propertyName ->
                        Ok <| propertyName

                    AddRelationshipList _ _ propertyName _ _ ->
                        Ok <| propertyName

                    RemoveRelationshipList _ _ propertyName _ ->
                        Ok <| propertyName

                    PositionRelationshipList _ _ propertyName _ ->
                        Ok <| propertyName

                    _ ->
                        errorResult

            NonMutating _ _ ->
                Err "Non-mutating events not supported"


{-|
    Get Target of event, i.e. Entity, Property, PropertyList, Relationship, RelationshipList.
-}
getTarget : Event -> Result String Target
getTarget event =
    case event of
        Mutating mutatingEventData _ ->
            case mutatingEventData of
                CreateEntity _ _ ->
                    Ok <| "entity"

                DestroyEntity _ _ ->
                    Ok <| "entity"

                AddProperty _ _ _ _ ->
                    Ok <| "property"

                RemoveProperty _ _ _ ->
                    Ok <| "property"

                AddPropertyList _ _ _ _ _ ->
                    Ok <| "propertyList"

                RemovePropertyList _ _ _ _ ->
                    Ok <| "propertyList"

                PositionPropertyList _ _ _ _ ->
                    Ok <| "propertyList"

                AddRelationship _ _ _ _ ->
                    Ok <| "relationship"

                RemoveRelationship _ _ _ ->
                    Ok <| "relationship"

                AddRelationshipList _ _ _ _ _ ->
                    Ok <| "relationshipList"

                RemoveRelationshipList _ _ _ _ ->
                    Ok <| "relationshipList"

                PositionRelationshipList _ _ _ _ ->
                    Ok <| "relationshipList"

        NonMutating _ _ ->
            Err "Non-mutating events not supported"


{-|
    Get Operation of event, e.g. created, destroyed, added, removed, positioned.
-}
getOperation : Event -> Result String Operation
getOperation event =
    case event of
        Mutating mutatingEventData _ ->
            case mutatingEventData of
                CreateEntity _ _ ->
                    Ok <| "created"

                DestroyEntity _ _ ->
                    Ok <| "destroyed"

                AddProperty _ _ _ _ ->
                    Ok <| "added"

                RemoveProperty _ _ _ ->
                    Ok <| "removed"

                AddPropertyList _ _ _ _ _ ->
                    Ok <| "added"

                RemovePropertyList _ _ _ _ ->
                    Ok <| "removed"

                PositionPropertyList _ _ _ _ ->
                    Ok <| "positioned"

                AddRelationship _ _ _ _ ->
                    Ok <| "added"

                RemoveRelationship _ _ _ ->
                    Ok <| "removed"

                AddRelationshipList _ _ _ _ _ ->
                    Ok <| "added"

                RemoveRelationshipList _ _ _ _ ->
                    Ok <| "removed"

                PositionRelationshipList _ _ _ _ ->
                    Ok <| "positioned"

        NonMutating _ _ ->
            Err "Non-mutating events not supported"


{-|
    Get Relationship from event.
-}
getRelationshipId : Event -> Result String RelationshipId
getRelationshipId event =
    let
        errorResult =
            Err ("Event:" +-+ event +-+ "doesn't have a relationshipId")
    in
        case event of
            Mutating mutatingEventData _ ->
                case mutatingEventData of
                    AddRelationship _ _ _ relationshipId ->
                        Ok <| relationshipId

                    AddRelationshipList _ _ _ _ relationshipId ->
                        Ok <| relationshipId

                    _ ->
                        errorResult

            NonMutating _ _ ->
                Err "Non-mutating events not supported"


{-|
    Get Position from event.
-}
getPosition : Event -> Result String Position
getPosition event =
    let
        errorResult =
            Err ("Event:" +-+ event +-+ "doesn't have a position")
    in
        case event of
            Mutating mutatingEventData _ ->
                case mutatingEventData of
                    PositionPropertyList _ _ _ position ->
                        Ok position

                    PositionRelationshipList _ _ _ position ->
                        Ok position

                    _ ->
                        errorResult

            NonMutating _ _ ->
                Err "Non-mutating events not supported"


{-|
    Get PropertyId from event.
-}
getPropertyId : Event -> Result String PropertyId
getPropertyId event =
    let
        errorResult =
            Err ("Event:" +-+ event +-+ "doesn't have a propertyId")
    in
        case event of
            Mutating mutatingEventData _ ->
                case mutatingEventData of
                    AddPropertyList _ _ _ propertyId _ ->
                        Ok propertyId

                    RemovePropertyList _ _ _ propertyId ->
                        Ok propertyId

                    AddRelationshipList _ _ _ propertyId _ ->
                        Ok propertyId

                    RemoveRelationshipList _ _ _ propertyId ->
                        Ok propertyId

                    _ ->
                        errorResult

            NonMutating _ _ ->
                Err "Non-mutating events not supported"


{-|
    Get InitiatorId from event's metadata.
-}
getInitiatorId : Event -> InitiatorId
getInitiatorId event =
    (getMetadata event).initiatorId


{-|
    Get Command from event's metadata.
-}
getCommand : Event -> String
getCommand event =
    (getMetadata event).command



-- PRIVATE API


getMetadata : Event -> Metadata
getMetadata event =
    case event of
        Mutating _ metadata ->
            metadata

        NonMutating _ metadata ->
            metadata


{-| Event decoder
-}
eventDecoder : JD.Decoder Event
eventDecoder =
    (JD.field "target" JD.string)
        |> JD.andThen
            (\type_ ->
                case type_ of
                    "none" ->
                        JD.succeed NonMutating
                            <|| (JD.field "value" JD.string)
                            <|| (JD.field "metadata" metadataDecoder)

                    _ ->
                        JD.succeed Mutating
                            <|| mutatingEventDecoder
                            <|| (JD.field "metadata" metadataDecoder)
            )


mutatingEventDecoder : JD.Decoder MutatingEvent
mutatingEventDecoder =
    JD.field "target" JD.string
        |> JD.andThen
            (\target ->
                JD.field "operation" JD.string
                    |> JD.andThen
                        (\operation ->
                            JD.field "entityName" JD.string
                                |> JD.andThen
                                    (\entityName ->
                                        JD.field "entityId" JD.string
                                            |> JD.andThen (\entityId -> JD.succeed ( target, operation, entityName, entityId ))
                                    )
                        )
            )
        |> JD.andThen
            (\( target, operation, entityName, entityId ) ->
                case ( target, operation ) of
                    ( "entity", "created" ) ->
                        JD.succeed <| CreateEntity entityName entityId

                    ( "entity", "destroyed" ) ->
                        JD.succeed <| DestroyEntity entityName entityId

                    ( "property", "added" ) ->
                        JD.succeed (AddProperty entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "value" JD.string)

                    ( "property", "removed" ) ->
                        JD.succeed (RemoveProperty entityName entityId)
                            <|| (JD.field "propertyName" JD.string)

                    ( "propertyList", "added" ) ->
                        JD.succeed (AddPropertyList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "propertyId" JD.string)
                            <|| (JD.field "value" JD.string)

                    ( "propertyList", "removed" ) ->
                        JD.succeed (RemovePropertyList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "propertyId" JD.string)

                    ( "propertyList", "positioned" ) ->
                        JD.succeed (PositionPropertyList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| positionDecoder

                    ( "relationship", "added" ) ->
                        JD.succeed (AddRelationship entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "relationshipId" JD.string)

                    ( "relationship", "removed" ) ->
                        JD.succeed (RemoveRelationship entityName entityId)
                            <|| (JD.field "propertyName" JD.string)

                    ( "relationshipList", "added" ) ->
                        JD.succeed (AddRelationshipList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "propertyId" JD.string)
                            <|| (JD.field "relationshipId" JD.string)

                    ( "relationshipList", "removed" ) ->
                        JD.succeed (RemoveRelationshipList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| (JD.field "propertyId" JD.string)

                    ( "relationshipList", "positioned" ) ->
                        JD.succeed (PositionRelationshipList entityName entityId)
                            <|| (JD.field "propertyName" JD.string)
                            <|| positionDecoder

                    _ ->
                        Debug.crash ("Unhandled MutatingEvent:" +-+ ( target, operation ))
            )


positionDecoder : JD.Decoder Position
positionDecoder =
    JD.field "oldPosition" JD.int
        |> JD.andThen
            (\oldPosition ->
                JD.field "newPosition" JD.int
                    |> JD.andThen (\newPosition -> JD.succeed ( oldPosition, newPosition ))
            )


metadataDecoder : JD.Decoder Metadata
metadataDecoder =
    JD.succeed Metadata
        <|| (JD.field "initiatorId" JD.string)
        <|| (JD.field "command" JD.string)


metadataEncoder : Metadata -> JE.Value
metadataEncoder metadata =
    JE.object
        [ ( "command", JE.string metadata.command )
        , ( "initiatorId", JE.string metadata.initiatorId )
        ]
