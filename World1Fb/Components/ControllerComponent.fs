module ControllerComponent
open CommonGenericFunctions
open ComponentEnums


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
        | Eat -> [| EatingComponentID |]
        | Idle -> [||]
        | Mate -> [| MatingComponentID |]
        | Move_East -> [| FormComponentID; MovementComponentID |]
        | Move_North -> [| FormComponentID; MovementComponentID |]
        | Move_South -> [| FormComponentID; MovementComponentID |]
        | Move_West -> [| FormComponentID; MovementComponentID |]


type ControllerTypes = 
    | AI_Random
    | Keyboard


type ControllerComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        ControllerType : ControllerTypes
        CurrentAction : ActionTypes
        CurrentActions : ActionTypes[]   // Actions that can be done this turn
        PotentialActions : ActionTypes[] // Actions that can be done by entities components
    }
    member me.ActionAllowed (action:ActionTypes) = me.CurrentActions |> Array.contains action

    member me.Update (controllerTypeUpdate:ControllerTypes option) (currentActionUpdate:ActionTypes option) (currentActionsUpdate:ActionTypes[] option) (potentialActionsUpdate:ActionTypes[] option) =
        {
            me with
                ControllerType = if controllerTypeUpdate.IsSome then controllerTypeUpdate.Value else me.ControllerType
                CurrentAction = if currentActionUpdate.IsSome then currentActionUpdate.Value else me.CurrentAction
                CurrentActions = if currentActionsUpdate.IsSome then currentActionsUpdate.Value else me.CurrentActions
                PotentialActions = if potentialActionsUpdate.IsSome then potentialActionsUpdate.Value else me.PotentialActions
        }

