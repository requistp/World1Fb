module MovementComponent
open System

[<Literal>]
let ComponentID_Movement = 3uy

type MovementDirection =
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

type MovementComponent = {
    MovesPerTurn : Byte
    }

type MovementComponent_Change = {
    Movements : MovementDirection list
    }