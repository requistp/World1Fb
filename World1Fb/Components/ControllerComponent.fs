module ControllerComponent
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
