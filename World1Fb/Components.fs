module Components
open LocationTypes

[<Literal>]
let ComponentID_Form = 1uy

[<Literal>]
let ComponentID_Terrain = 2uy

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
type FormComponent_Change(isPassable:bool option, name:string option, symbol:char option, location:LocationDataFloat option) =
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
type TerrainComponent_Change(terrainType:TerrainType option) =
    member this.TerrainType = terrainType

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

[<Struct>]
type ComponentChangeLog(log:Map<uint32,ComponentChangeType list>) =
    member this.Log = log
