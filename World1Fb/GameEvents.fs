module GameEvents
open AbstractComponent
open MovementComponent
open TerrainComponent

type GameEventTypes =
    | Component_Created_Terrain
    | Movement
    | Movement_KeyPressed


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et


type Event_ComponentCreated_Terrain(eid:uint32, ct:TerrainComponent) =
    inherit AbstractGameEvent(Component_Created_Terrain)
    member _.EntityID = eid
    member _.Terrain = ct


type Event_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement)
    member _.Direction = direction
    member _.EntityID = eid
    

type Event_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed)
    member _.Direction = direction
    member _.EntityID = eid

    
   