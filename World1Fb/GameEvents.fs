module GameEvents
open AbstractComponent
open MovementComponent
open TerrainComponent


type GameEventTypes =
    | Eat
    | Eaten
    | KeyPressed_Eat
    | KeyPressed_Movement
    | Movement


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et


type Event_Eat(eaterID:uint32, eateeID:uint32) =
    inherit AbstractGameEvent(Eat)
    member _.EaterID = eaterID
    member _.EateeID = eateeID
    

type Event_Eaten(eateeID:uint32, eaterID:uint32, quantity:int) =
    inherit AbstractGameEvent(Eaten)
    member _.EaterID = eaterID
    member _.EateeID = eateeID
    member _.Quantity = quantity
    

type Event_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement)
    member _.Direction = direction
    member _.EntityID = eid
    

type Event_KeyPressed_Eat(eid:uint32) =
    inherit AbstractGameEvent(KeyPressed_Eat)
    member _.EntityID = eid


type Event_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(KeyPressed_Movement)
    member _.Direction = direction
    member _.EntityID = eid

    
   