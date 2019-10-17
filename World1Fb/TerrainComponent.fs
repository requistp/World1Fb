module TerrainComponent
open AbstractComponent
open LocationTypes


type TerrainType = 
    | Dirt 
    | Rock
    | Sand
    member this.IsPassable = 
        match this with
        | Dirt | Sand -> true
        | Rock -> false
    member this.Symbol = 
        match this with
        | Dirt -> '.'
        | Sand -> ','
        | Rock -> '#'


type TerrainComponent(eid:uint32, terrainType:TerrainType) =
    inherit AbstractComponent(eid,Comp_Terrain)

    member _.Terrain = terrainType

    static member Type = Comp_Terrain

