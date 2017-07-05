module Slate.Common.StateMachine
    exposing
        ( StateMachine
        , validateStateMachine
        , validateTransition
        )

{-| State machine functions

@docs StateMachine , validateStateMachine , validateTransition

-}

import Set
import Tuple exposing (..)
import StringUtils exposing (..)
import Utils.Ops exposing (..)


{-| state machine
-}
type alias StateMachine state =
    { makeComparable : state -> String
    , initialStates : List state
    , edges : List ( state, state )
    , terminalStates : List state
    }


{-| validate state machine
-}
validateStateMachine : StateMachine state -> StateMachine state
validateStateMachine stateMachine =
    (stateMachine.initialStates == [])
        ?! ( (\_ -> Debug.crash "State machine must have non-empty initialStates")
           , always
                (( stateMachine.edges
                    |> List.map (stateMachine.makeComparable << first)
                    |> Set.fromList
                 , stateMachine.edges |> List.map (stateMachine.makeComparable << second) |> Set.fromList
                 )
                    |> (\( first, second ) ->
                            Set.diff (Set.fromList <| List.map stateMachine.makeComparable stateMachine.initialStates) first
                                |> (\diff ->
                                        (Set.size diff == 0)
                                            ?! ( always
                                                    (Set.diff (Set.diff second <| Set.fromList <| List.map stateMachine.makeComparable stateMachine.terminalStates) first
                                                        |> (\diff ->
                                                                (Set.size diff == 0)
                                                                    ?! ( always stateMachine, (\_ -> Debug.crash ("Bad transition(s) in state machine to non-existing states:" +-+ diff)) )
                                                           )
                                                    )
                                               , (\_ -> Debug.crash ("Bad initialStates in stateMachine:" +-+ diff))
                                               )
                                   )
                       )
                )
           )


{-| helper for change validators in schemas
-}
validateTransition : StateMachine state -> Maybe state -> state -> Result String ()
validateTransition stateMachine maybeCurrentState newState =
    maybeCurrentState
        |?> (\currentState ->
                (stateMachine.edges
                    |> List.filter ((==) currentState << first)
                    |> List.filter ((==) newState << second)
                    |> List.length
                    |> (/=) 0
                )
                    ? ( Ok ()
                      , Err ("Invalid transition from:" +-+ currentState +-+ "to:" +-+ newState)
                      )
            )
        ?= (List.member newState stateMachine.initialStates
                ? ( Ok ()
                  , Err ("Invalid initial state:" +-+ newState)
                  )
           )
