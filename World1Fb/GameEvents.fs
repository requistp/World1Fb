module GameEvents
open AbstractComponent
open MovementComponent
open TerrainComponent


type GameEventTypes =
    | Eaten
    | Movement
    | Movement_KeyPressed


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et


type Event_Eaten(eid:uint32, eatorID:uint32, quantity:int) =
    inherit AbstractGameEvent(Eaten)
    member _.EatorID = eatorID
    member _.EntityID = eid
    member _.Quantity = quantity
    

type Event_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement)
    member _.Direction = direction
    member _.EntityID = eid
    

type Event_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed)
    member _.Direction = direction
    member _.EntityID = eid

    
   