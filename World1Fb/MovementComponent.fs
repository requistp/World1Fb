module MovementComponent
open AbstractComponent
open LocationTypes

[<Literal>]
let ComponentID_Movement = 3uy

type MovementDirection =
    | North
    | East
    | South
    | West
    member this.X_change = 
        match this with
        | North | South -> 0
        | East -> 1
        | West -> -1
    member this.Y_change = 
        match this with
        | East | West -> 0
        | North -> -1
        | South -> 1
    member this.AddToLocation (location:LocationDataInt) =
        { X = location.X + this.X_change; Y = location.Y + this.Y_change }


type MovementComponent(movesPerTurn:int) =
    inherit AbstractComponent(ComponentID_Movement)
    member _.MovesPerTurn = movesPerTurn
    

type MovementComponent_Change(eid:uint32, moveDirection:MovementDirection, x:int, y:int) =
    inherit AbstractComponent_Change(eid)
    member _.Movement = moveDirection
    member _.X = x
    member _.Y = y

type MovementComponent_ChangeSum(eid:uint32, x:int, y:int) =
    inherit AbstractComponent_ChangeSum(eid)
    member _.X = x
    member _.Y = y

