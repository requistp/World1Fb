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

type TerrainComponent(terrainType:TerrainType, location:LocationDataInt) =
    member this.IsPassable = TerrainIsPassable terrainType
    member this.TerrainType = terrainType
    member this.Symbol = TerrainSymbol terrainType
    member this.Location = location

type TerrainComponent_Change(terrainType:TerrainType option) =
    member this.TerrainType = terrainType
