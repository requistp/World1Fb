module Components
open LocationTypes

type TerrainType = 
    | Dirt 
    | Rock
    | Sand
let TerrainSymbol tt = 
    match tt with
    | Dirt -> '.'
    | Sand -> ','
    | Rock -> '#'
let TerrainIsPassable tt = 
    match tt with
    | Dirt | Sand -> true
    | Rock -> false

[<Struct>]
type FormComponent(isPassable:bool, name:string, symbol:char, location:LocationDataFloat) =
    member this.IsPassable = isPassable
    member this.Name = name
    member this.Symbol = symbol
    member this.Location = location

[<Struct>]
type TerrainComponent(terrainType:TerrainType, location:LocationDataInt) =
    member this.IsPassable = TerrainIsPassable terrainType
    member this.TerrainType = terrainType
    member this.Symbol = TerrainSymbol terrainType
    member this.Location = location

[<Struct>]
type ComponentType = 
    | Form of Form:FormComponent
    | Terrain of Terrain:TerrainComponent
    member this.ComponentID = 
        match this with
        | Form _ -> 1
        | Terrain _ -> 2

[<Struct>]
type EntityComponent(eid:uint32, comp:ComponentType) =
    member this.EntityID = eid
    member this.Component = comp
