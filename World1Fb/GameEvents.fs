module GameEvents
open AbstractComponent
open MovementComponent
open TerrainComponent


type GameEventTypes =
    | Action_Eat
    //| Eaten
    //| KeyPressed_Eat
    | Action_Movement
    | CreateEntity
    | Movement


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et

type Event_Action_Eat(eaterID:uint32, eateeID:uint32) =
    inherit AbstractGameEvent(Action_Eat)
    member _.EaterID = eaterID
    member _.EateeID = eateeID
    
type Event_Action_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Action_Movement)
    member _.Direction = direction
    member _.EntityID = eid
    
type Event_CreateEntity(cts:AbstractComponent[]) =
    inherit AbstractGameEvent(CreateEntity)
    member _.Components = cts
         
//type Event_Eaten(eateeID:uint32, eaterID:uint32, quantity:int) =
//    inherit AbstractGameEvent(Eaten)
//    member _.EaterID = eaterID
//    member _.EateeID = eateeID
//    member _.Quantity = quantity
    

type Event_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement)
    member _.Direction = direction
    member _.EntityID = eid
    

//type Event_KeyPressed_Eat(eid:uint32) =
//    inherit AbstractGameEvent(KeyPressed_Eat)
//    member _.EntityID = eid



    
   