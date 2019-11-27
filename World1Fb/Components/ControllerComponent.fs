module ControllerComponent
open CommonGenericFunctions
open ComponentEnums

type ControllerTypes = 
    | AI_Random
    | Keyboard

type ActionTypes = 
    | Eat
    | Idle
    | Mate
    | Move_East
    | Move_North
    | Move_South
    | Move_West
    static member AsArray = 
        [|
            Eat
            Idle
            Mate
            Move_East
            Move_North
            Move_South
            Move_West
        |]
    member me.RequiredComponents =
        match me with
        | Eat -> [| EatingComponentType |]
        | Idle -> [||]
        | Mate -> [| MatingComponentType |]
        | Move_East -> [| FormComponentType; MovementComponentType |]
        | Move_North -> [| FormComponentType; MovementComponentType |]
        | Move_South -> [| FormComponentType; MovementComponentType |]
        | Move_West -> [| FormComponentType; MovementComponentType |]

[<Struct>]
type ControllerComponent(id:ComponentID, eid:EntityID, controllerType:ControllerTypes, currentAction:ActionTypes, currentActions:ActionTypes[], potentialActions:ActionTypes[]) =
    member _.ID = id
    member _.EntityID = eid
    member _.ControllerType = controllerType
    member _.CurrentAction = currentAction
    member _.CurrentActions = currentActions
    member _.PotentialActions = potentialActions

let ActionIsAllowed (cc:ControllerComponent) (action:ActionTypes) = cc.CurrentActions |> Array.contains action

//let UpdateController (cc:ControllerComponent) (controllerTypeUpdate:ControllerTypes option) (currentActionUpdate:ActionTypes option) (currentActionsUpdate:ActionTypes[] option) (potentialActionsUpdate:ActionTypes[] option) =
//    {
//        cc with
//            ControllerType = if controllerTypeUpdate.IsSome then controllerTypeUpdate.Value else cc.ControllerType
//            CurrentAction = if currentActionUpdate.IsSome then currentActionUpdate.Value else cc.CurrentAction
//            CurrentActions = if currentActionsUpdate.IsSome then currentActionsUpdate.Value else cc.CurrentActions
//            PotentialActions = if potentialActionsUpdate.IsSome then potentialActionsUpdate.Value else cc.PotentialActions
//    }


