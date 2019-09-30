module TerrainComponent
open LocationTypes

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

type TerrainComponent = {
    Type : TerrainType
    Location : LocationDataInt
    } with
    member this.IsPassable = TerrainIsPassable this.Type
    member this.Symbol = TerrainSymbol this.Type

type TerrainComponent_Change = {
    Type : TerrainType option
    }
