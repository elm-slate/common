module Slate.Common.Mutation
    exposing
        ( MutationId
        , MutateFunction
        , MutateCascadingDeleteFunction
        , MutationTagger
        , CascadingDeletionTaggers
        , CascadingDelete
        , RelationshipIdAccessDict
        , buildCascadingDelete
        , processMutationResult
        , processCascadingMutationResult
        , relationshipIdAccessor
        , relationshipListIdsAccessor
        , getEventType
        , updatePropertyValue
        , updatePropertyRelationship
        , addToPropertyList
        , removeFromPropertyList
        , addToRelationshipList
        , removeFromRelationshipList
        , positionRelationshipList
        , positionPropertyList
        , handleMutation
        , handleMutationCascadingDelete
        )

{-| Slate Projection helpers.

    This module contains helpers for projection processing.

@docs MutationId, MutateFunction, MutateCascadingDeleteFunction, MutationTagger, CascadingDeletionTaggers, CascadingDelete, RelationshipIdAccessDict, buildCascadingDelete, processMutationResult, processCascadingMutationResult, relationshipIdAccessor, relationshipListIdsAccessor, getEventType, updatePropertyValue, updatePropertyRelationship, addToPropertyList, removeFromPropertyList, addToRelationshipList, removeFromRelationshipList, positionPropertyList, positionRelationshipList, handleMutation, handleMutationCascadingDelete

-}

import Dict exposing (Dict)
import List.Extra as ListE
import Slate.Common.Entity exposing (..)
import Slate.Common.Event as Event exposing (..)
import Slate.Common.Schema as Schema exposing (..)
import Utils.Ops exposing (..)
import StringUtils exposing (..)
import DebugF exposing (toStringF)


{-| Mutation identifier.
-}
type alias MutationId =
    Int


{-| Signature for mutate function
-}
type alias MutateFunction entityFragment =
    Event -> entityFragment -> Result String (Maybe entityFragment)


{-| Signature for cascading delete mutate function
-}
type alias MutateCascadingDeleteFunction entityFragment =
    Event -> entityFragment -> ( Result String (Maybe entityFragment), Maybe CascadingDelete )


{-| Mutation tagger.
-}
type alias MutationTagger msg =
    MutationId -> EventRecord -> msg


{-| Cascading deletion taggers.
-}
type alias CascadingDeletionTaggers msg =
    Dict EntityName (MutationTagger msg)


{-| Cascading Delete definition.
-}
type alias CascadingDelete =
    List ( EntityName, List RelationshipId )


{-| RelationshipId Access Dict.
-}
type alias RelationshipIdAccessDict entityFragment =
    Dict PropertyName (RelationshipIdsAccessor entityFragment)



----------------------------------------------------------
-- Event Procesing Helpers
----------------------------------------------------------


{-| Build cascading delete list based on the `owned` field of the property schema.
-}
buildCascadingDelete : EntitySchema -> RelationshipIdAccessDict entity -> entity -> CascadingDelete
buildCascadingDelete entitySchema relationshipIdAccessDict entity =
    let
        ownedPropertySchemas =
            entitySchema.properties
                |> List.filter isPropertyOwned

        ownedPropertyNames =
            ownedPropertySchemas
                |> List.map Schema.getPropertyName

        ownedPropertyEntityNames =
            ownedPropertySchemas
                |> List.map (\propertySchema -> Schema.getPropertyEntitySchema propertySchema |?> .entityName ?= "")
                |> List.filter ((/=) "")

        relationshipIdsList =
            ownedPropertyNames
                |> List.map (\propertyName -> Dict.get propertyName relationshipIdAccessDict ?= (\_ -> []))
                |> List.map ((|>) entity)
    in
        List.map2 (,) ownedPropertyEntityNames relationshipIdsList


{-| Helper for processing a non-cascading delete Mutation result from an Entity's handleMutation function that returns:
        Result String (EntityDict entity)
-}
processMutationResult : model -> (model -> EntityDict entity -> model) -> (model -> (String -> ( ( model, Cmd msg ), List parentMsg ))) -> Result String (EntityDict entity) -> ( ( model, Cmd msg ), List parentMsg )
processMutationResult model modelMutator errorHandler result =
    result
        |??> (\newDict -> ( modelMutator model newDict ! [], [] ))
        ??= errorHandler model


{-| Helper for processing a Cascading Delete Mutation result from an Entity's handleMutation function that returns:
        ( Result String (EntityDict entity), Maybe CascadingDelete )
-}
processCascadingMutationResult : model -> CascadingDeletionTaggers msg -> (msg -> model -> ( ( model, Cmd msg ), List parentMsg )) -> MutationId -> EventRecord -> (model -> EntityDict entity -> model) -> (model -> (String -> ( ( model, Cmd msg ), List parentMsg ))) -> Result String ( EntityDict entity, Maybe CascadingDelete ) -> ( ( ( model, Cmd msg ), List parentMsg ), Maybe CascadingDelete )
processCascadingMutationResult model deleteTaggers update mutationId eventRecord modelMutator errorHandler mutationResult =
    mutationResult
        |??>
            (\( newDict, maybeCascadingDelete ) ->
                let
                    mutatedModel =
                        modelMutator model newDict
                in
                    (maybeCascadingDelete
                        |?> (\cascadingDelete ->
                                let
                                    ( ( finalModel, cmds ), msgs ) =
                                        buildCascadingDeleteMsgs mutationId eventRecord deleteTaggers cascadingDelete
                                            |> List.foldl
                                                (\msg ( ( model, cmds ), msgs ) ->
                                                    let
                                                        ( ( newModel, cmd ), newMsgs ) =
                                                            update msg model
                                                    in
                                                        ( ( newModel, cmd :: cmds ), List.append msgs newMsgs )
                                                )
                                                ( ( mutatedModel, [] ), [] )
                                in
                                    ( ( finalModel ! cmds, msgs ), Just cascadingDelete )
                            )
                        ?= ( ( (mutatedModel ! []), [] ), Nothing )
                    )
            )
        ??= (\error -> ( errorHandler model error, Nothing ))


{-| Helper to access a single relationshipId from an entityFragment.
-}
relationshipIdAccessor : (entityFragment -> Maybe RelationshipId) -> RelationshipIdsAccessor entityFragment
relationshipIdAccessor propertyAccessor entityFragment =
    (propertyAccessor entityFragment) |?> (\accessor -> [ accessor ]) ?= []


{-| Helper to access mutliple relationshipIds from an entityFragment.
-}
relationshipListIdsAccessor : (entityFragment -> PropertyList RelationshipId) -> RelationshipIdsAccessor entityFragment
relationshipListIdsAccessor propertyListAccessor =
    .items << propertyListAccessor



----------------------------------------------------------
-- Entity Mutation Helpers
----------------------------------------------------------


{-| Get info that identifies the Event type.
-}
getEventType : Event -> EventType
getEventType event =
    let
        maybePropertyName =
            Event.getPropertyName event |??> (Just << identity) ??= (\_ -> Nothing)

        ( target, operation ) =
            ( (getTarget event) ??= crash, (getOperation event) ??= crash )
    in
        ( target, operation, maybePropertyName )


{-| Update Entire Entity property value.
-}
updatePropertyValue : (Event -> Result String value) -> (Maybe value -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
updatePropertyValue valueGetter propertySetter event entityFragment =
    let
        value =
            valueGetter event
    in
        value |??> (\val -> Ok (propertySetter (Just val) entityFragment)) ??= Err


{-| Update Entire Entity property relationship.
-}
updatePropertyRelationship : (Maybe EntityId -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
updatePropertyRelationship =
    updatePropertyValue getRelationshipId


{-| Add item to Entire Entity property list. This is done by appending since positioning is done by another event.
-}
addToPropertyList : (entityFragment -> PropertyList listValue) -> (Event -> Result String listValue) -> (PropertyList listValue -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
addToPropertyList listGetter valueGetter propertySetter event entityFragment =
    let
        listValue =
            valueGetter event

        list =
            listGetter entityFragment
    in
        listValue
            |??>
                (\listVal ->
                    getPropertyId event
                        |??>
                            (\propertyId ->
                                let
                                    newList =
                                        { list | ids = List.append list.ids [ propertyId ], items = List.append list.items [ listVal ] }
                                in
                                    Ok (propertySetter newList entityFragment)
                            )
                        ??= Err
                )
            ??= Err


{-| Remove item from Entire Entity property list.
-}
removeFromPropertyList : (entityFragment -> PropertyList listValue) -> (Event -> Result String listValue) -> (PropertyList listValue -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
removeFromPropertyList listGetter valueGetter propertySetter event entityFragment =
    let
        listValue =
            valueGetter event

        list =
            listGetter entityFragment
    in
        listValue
            |??>
                (\listVal ->
                    getPropertyId event
                        |??>
                            (\propertyId ->
                                ListE.elemIndex propertyId list.ids
                                    |?> (\index ->
                                            let
                                                newList =
                                                    { list | ids = ListE.removeAt index list.ids, items = ListE.removeAt index list.items }
                                            in
                                                Ok (propertySetter newList entityFragment)
                                        )
                                    ?= Err ("Event:" +-+ (toStringF event) +-+ "is has propertyId:" +-+ (toStringF propertyId) +-+ "that's not in list:" +-+ (toStringF list))
                            )
                        ??= Err
                )
            ??= Err


{-| Add relationship to Entire Entity relationship list. This is done by appending since positioning is done by another event.
-}
addToRelationshipList : (entityFragment -> PropertyList EntityId) -> (PropertyList EntityId -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
addToRelationshipList listGetter =
    addToPropertyList listGetter getRelationshipId


{-| Remove relationship from Entire Entity relationship list.
-}
removeFromRelationshipList : (entityFragment -> PropertyList EntityId) -> (PropertyList EntityId -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
removeFromRelationshipList listGetter =
    removeFromPropertyList listGetter getRelationshipId


{-| Position Entire Entity property list where the property at `event.data.oldPosition` is FIRST removed and then inserted to `event.data.newPosition`, e.g.:

    	A B [C] D E F G
    	oldPosition = 2
    	A B D E F G
    	newPosition = 3
    	A B D [C] E F G
-}
positionPropertyList : (entityFragment -> PropertyList listValue) -> (PropertyList listValue -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
positionPropertyList listGetter propertySetter event entityFragment =
    let
        list =
            listGetter entityFragment

        maybePositionResult =
            getPosition event

        length =
            List.length list.ids

        position list oldPosition newPosition =
            let
                item =
                    List.take 1 (List.drop oldPosition list)

                itemRemovedList =
                    List.append (List.take oldPosition list) (List.drop (oldPosition + 1) list)
            in
                List.append (List.take newPosition itemRemovedList) (List.append item <| List.drop newPosition itemRemovedList)
    in
        maybePositionResult
            |??>
                (\( oldPosition, newPosition ) ->
                    not (oldPosition >= length || newPosition >= length || oldPosition < 0 || newPosition < 0)
                        ? ( Ok <| propertySetter { ids = position list.ids oldPosition newPosition, items = position list.items oldPosition newPosition } entityFragment
                          , Err "Positions are out of bound"
                          )
                )
            ??= Err


{-| Position Entire Entity relationship list where the property at `event.data.oldPosition` is FIRST removed and then inserted to `event.data.newPosition`, e.g.:

    	A B [C] D E F G
    	oldPosition = 2
    	A B D E F G
    	newPosition = 3
    	A B D [C] E F G
-}
positionRelationshipList : (entityFragment -> PropertyList EntityId) -> (PropertyList EntityId -> entityFragment -> entityFragment) -> Event -> entityFragment -> Result String entityFragment
positionRelationshipList =
    positionPropertyList


{-| Mutate Entire Entity Dictionary based on an event.
-}
handleMutation : entityFragment -> MutateFunction entityFragment -> EntityDict entityFragment -> Event -> Result String (EntityDict entityFragment)
handleMutation entireEntityShell mutate dict event =
    (getEntityId event)
        |??>
            (\entityId ->
                let
                    entity =
                        Dict.get entityId dict
                            ?= entireEntityShell
                in
                    (mutate event entity)
                        |??>
                            (\maybeAddress ->
                                maybeAddress |?> (\address -> Dict.insert entityId address dict) ?= Dict.remove entityId dict
                            )
            )
        ??= Err


{-| Mutate Entire Entity Dictionary based on an event with CascadingDelete support.
-}
handleMutationCascadingDelete : entityFragment -> MutateCascadingDeleteFunction entityFragment -> EntityDict entityFragment -> Event -> Result String ( EntityDict entityFragment, Maybe CascadingDelete )
handleMutationCascadingDelete entireEntityShell mutate dict event =
    (getEntityId event)
        |??>
            (\entityId ->
                let
                    entity =
                        Dict.get entityId dict
                            ?= entireEntityShell

                    ( mutationResult, maybeDelete ) =
                        mutate event entity
                in
                    mutationResult
                        |??>
                            (\maybePerson ->
                                ( maybePerson
                                    |?> (\person -> Dict.insert entityId person dict)
                                    ?= Dict.remove entityId dict
                                , maybeDelete
                                )
                            )
            )
        ??= Err



-- PRIVATE API


type alias RelationshipIdsAccessor entityFragment =
    entityFragment -> List RelationshipId


crash : String -> a
crash error =
    Debug.crash ("Program bug:" +-+ error)


{-| Build projection Msgs for cascading deletion.
-}
buildCascadingDeleteMsgs : MutationId -> EventRecord -> CascadingDeletionTaggers msg -> CascadingDelete -> List msg
buildCascadingDeleteMsgs mutationId originatingEventRecord deletionTaggers cascadingDelete =
    let
        buildCascadingDeleteMsg ( entityName, relationshipIdsList ) =
            let
                maybeDeletionTagger =
                    Dict.get entityName deletionTaggers

                mutatingEventData =
                    case originatingEventRecord.event of
                        Mutating mutatingEventData _ ->
                            mutatingEventData

                        NonMutating _ _ ->
                            crash "BUG -- Non-mutating events cannot create cascading deletes"

                event =
                    originatingEventRecord.event
            in
                maybeDeletionTagger
                    |?> (\deletionTagger ->
                            relationshipIdsList
                                |> List.map
                                    (\entityId ->
                                        deletionTagger mutationId
                                            { id = "Cascading:" ++ originatingEventRecord.id
                                            , ts = originatingEventRecord.ts
                                            , event = Mutating (DestroyEntity entityName entityId) <| Metadata (getInitiatorId event) (getCommand event)
                                            , max = Nothing
                                            }
                                    )
                        )
                    ?!= (\_ -> crash ("Missing Deletion Tagger for:" +-+ entityName))
    in
        cascadingDelete
            |> List.map buildCascadingDeleteMsg
            |> List.concat
