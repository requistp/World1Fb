﻿module ControllerComponent
open ComponentEnums


type ActionTypes = 
    | Eat
    | Mate
    | Move
    static member AsArray = 
        [|
            Eat
            Mate
            Move
        |]
    member me.RequiredComponents =
        match me with
        | Eat -> [| EatingComponentID |]
        | Mate -> [| MatingComponentID |]
        | Move -> [| FormComponentID; MovementComponentID |]


type ControllerComponent = 
    { 
        EntityID : uint32
        Actions : ActionTypes[]
        CurrentActions : ActionTypes[]
    }
    member me.Update (actionsUpdate:ActionTypes[] option) (currentActionsUpdate:ActionTypes[] option) =
        {
            me with
                Actions = if actionsUpdate.IsSome then actionsUpdate.Value else me.Actions
                CurrentActions = if currentActionsUpdate.IsSome then currentActionsUpdate.Value else me.CurrentActions
        }
