module GameEvents
open MovementComponent

type GameEventTypes =
    //| KeyPressed
    | Movement_KeyPressed
    | Movement


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et


type GameEventData_Movement_KeyPressed(direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed)
    member _.Direction = direction
