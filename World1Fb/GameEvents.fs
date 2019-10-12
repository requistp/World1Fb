module GameEvents
open AbstractComponent
open MovementComponent
open TerrainComponent

type GameEventTypes =
    | Component_Created_Terrain
    | Entity_Create
    | Entity_ComponentChange
    | Movement
    | Movement_KeyPressed
    | SystemChangeLog


[<AbstractClass>]
type AbstractGameEvent(et:GameEventTypes) =
    member _.GameEventType = et


type Event_ComponentCreated_Terrain(eid:uint32, ct:TerrainComponent) =
    inherit AbstractGameEvent(Component_Created_Terrain)
    member _.EntityID = eid
    member _.Terrain = ct


type Event_Entity_Creates(acs:AbstractComponent[][]) =
    inherit AbstractGameEvent(Entity_Create)
    member _.Components = acs


type Event_Entity_ComponentChanges(acc:AbstractComponentChange[]) =
    inherit AbstractGameEvent(Entity_ComponentChange)
    member _.ComponentChange = acc
    

type Event_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement)
    member _.Direction = direction
    member _.EntityID = eid
    

type Event_KeyPressed_Movement(eid:uint32, direction:MovementDirection) =
    inherit AbstractGameEvent(Movement_KeyPressed)
    member _.Direction = direction
    member _.EntityID = eid


type Event_SystemChangeLog(scl:SystemChangeLog) =
    inherit AbstractGameEvent(SystemChangeLog)
    member _.SCL = scl
    