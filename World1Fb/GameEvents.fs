module GameEvents
open MovementComponent
open TerrainComponent

type GameEventTypes =
    | ComponentCreated_Terrain
    | Movement
    | Movement_KeyPressed


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes, eid:uint32) =
    member _.GameEventType = et
    member _.EntityID = eid


type GameEvent_ComponentCreated_Terrain(eid:uint32, ct:TerrainComponent) =
    inherit AbstractGameEvent(ComponentCreated_Terrain,eid)
    member _.Terrain = ct


type GameEvent_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement,eid)
    member _.Direction = direction
    

type GameEvent_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed,eid)
    member _.Direction = direction


