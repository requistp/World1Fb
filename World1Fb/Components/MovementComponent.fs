module MovementComponent
open AbstractComponent
open LocationTypes
open System


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
    member this.Z_change = 
        match this with
        | _ -> 0
    member this.AddToLocation (l:LocationDataInt) =
        { 
            X = byte (int l.X + this.X_change)
            Y = byte (int l.Y + this.Y_change)
            Z = byte (int l.Z + this.Z_change) 
        }
    

type MovementComponent(eid:uint32, movesPerTurn:int) =
    inherit AbstractComponent(eid,Component_Movement)

    member _.MovesPerTurn = movesPerTurn


    override this.Copy neweid = 
        MovementComponent(neweid, movesPerTurn).Abstract
