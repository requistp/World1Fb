module Components
open FormComponent
open TerrainComponent

[<Struct>]
type ComponentType = 
    | Form of Form:FormComponent
    | Terrain of Terrain:TerrainComponent
    member this.ComponentID = 
        match this with
        | Form _ -> ComponentID_Form
        | Terrain _ -> ComponentID_Terrain

[<Struct>]
type ComponentChangeType = 
    | FormChange of Form:FormComponent_Change
    | TerrainChange of Terrain:TerrainComponent_Change
    member this.ComponentID = 
        match this with
        | FormChange _ -> ComponentID_Form
        | TerrainChange _ -> ComponentID_Terrain

[<Struct>]
type EntityAdditionType (ctl:ComponentType list) =
    member this.Components = ctl

[<Struct>]
type EntityRemovalType (e:uint32) =
    member this.EntityID = e

[<Struct>]
type ChangeTypes = 
    | Component of Component:ComponentChangeType
    | EntityAddition of EntityAddition:EntityAdditionType
    | EntityRemoval of EntityRemoval:EntityRemovalType

[<Struct>]
type EntityComponent(eid:uint32, comp:ComponentType) =
    member this.EntityID = eid
    member this.Component = comp
