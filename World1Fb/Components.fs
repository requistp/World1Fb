module Components
open FormComponent
open TerrainComponent

type ComponentType = 
    | Form of FormComponent
    | Terrain of TerrainComponent
    member this.ComponentID = 
        match this with
        | Form _ -> ComponentID_Form
        | Terrain _ -> ComponentID_Terrain

type ComponentChangeType = 
    | FormChange of FormComponent_Change
    | TerrainChange of TerrainComponent_Change
    member this.ComponentID = 
        match this with
        | FormChange _ -> ComponentID_Form
        | TerrainChange _ -> ComponentID_Terrain

type EntityComponentChange = 
    | ComponentChange of ComponentChangeType
    | EntityAddition of ComponentType list
    | EntityRemoval of uint32

type EntityComponent(eid:uint32, comp:ComponentType) =
    member this.EntityID = eid
    member this.Component = comp
