module Slate.Common.Projection
    exposing
        ( ProjectionErrors
        , DictProjectionErrors
        , projectEntity
        , projectMap
        , successfulProjections
        , failedProjections
        , allFailedProjections
        )

{-|
    Slate Projection helpers.

    This module contains helpers for projection processing.

@docs  ProjectionErrors, DictProjectionErrors, projectEntity, projectMap, successfulProjections, failedProjections, allFailedProjections
-}

import Dict exposing (Dict)
import Tuple exposing (first, second)
import Result.Extra as ResultE exposing (isOk)
import Utils.Ops exposing (..)
import Utils.Dict as DictU
import Utils.Result as ResultU


{-|
    Projection error messages.
-}
type alias ProjectionErrors =
    List String


{-|
    Projection errors for a dictionary or entities.
-}
type alias DictProjectionErrors =
    List (List String)


{-|
    Get a valid entity or errors depending on the supplied errorChecks.
-}
projectEntity : List ( Bool, String ) -> entity -> Result (ProjectionErrors) entity
projectEntity errorChecks entity =
    let
        errors =
            errorChecks
                |> List.filter first
                |> List.map second
    in
        (errors == []) ? ( Ok entity, Err errors )


{-|
    Creates a Dictionary map function for projections from a simple projection function.
-}
projectMap : (entityFragment -> partialEntity) -> (Dict comparable entityFragment -> Dict comparable partialEntity)
projectMap f =
    Dict.map (\_ value -> f value)


{-|
    Filter successful projections.
-}
successfulProjections : Dict comparable (Result (ProjectionErrors) a) -> Dict comparable a
successfulProjections dictResult =
    let
        okDict =
            Dict.filter (\_ maybe -> isOk maybe) dictResult
    in
        DictU.zip (Dict.keys okDict)
            (ResultU.filterOk <| Dict.values okDict)


{-|
    Filter failed projections.
-}
failedProjections : Dict comparable (Result (ProjectionErrors) b) -> DictProjectionErrors
failedProjections =
    ResultU.filterErr << Dict.values


{-|
    Filter all failed projections from a list of projection results.
-}
allFailedProjections : List (DictProjectionErrors) -> ProjectionErrors
allFailedProjections =
    List.concat << (List.map List.concat)
