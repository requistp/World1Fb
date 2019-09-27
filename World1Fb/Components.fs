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
type ComponentChangeLog(log:Map<uint32,ComponentChangeType list>) =
    member this.Log = log

[<Struct>]
type EntityChangeLog(additions:Map<uint32,ComponentType list>, removals:uint32 list) =
    member this.Additions = additions
    member this.Removals = removals

[<Struct>]
type EntityComponent(eid:uint32, comp:ComponentType) =
    member this.EntityID = eid
    member this.Component = comp
