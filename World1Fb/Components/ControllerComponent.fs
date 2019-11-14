module ControllerComponent
open ComponentEnums


type ActionTypes = 
    | Eat
    | Mate
    | Move_North
    | Move_East
    | Move_South
    | Move_West
    static member AsArray = 
        [|
            Eat
            Mate
            Move_North
            Move_East
            Move_South
            Move_West
        |]
    member me.RequiredComponents =
        match me with
        | Eat -> [| EatingComponentID |]
        | Mate -> [| MatingComponentID |]
        | Move_North -> [| FormComponentID; MovementComponentID |]
        | Move_East -> [| FormComponentID; MovementComponentID |]
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
