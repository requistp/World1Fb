module MovementComponent
open AbstractComponent
open LocationTypes


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


type MovementComponent(eid:uint32, movesPerTurn:int) =
    inherit AbstractComponent(eid,Movement)

    member _.MovesPerTurn = movesPerTurn

    override this.NewWithEID eid = MovementComponent(eid, this.MovesPerTurn) :> AbstractComponent

    new(movesPerTurn:int) = MovementComponent(0u,movesPerTurn)

