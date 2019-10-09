module GameEvents
open MovementComponent

type GameEventTypes =
    | Movement_KeyPressed
    | Movement


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes, eid:uint32) =
    member _.GameEventType = et
    member _.EntityID = eid


type GameEvent_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed,eid)
    member _.Direction = direction


type GameEvent_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement,eid)
    member _.Direction = direction
    
