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
    inherit AbstractComponent(eid,Component_Terrain)
    static member Type = Component_Terrain

    member _.Terrain = terrainType

    override this.Copy neweid = 
        TerrainComponent(neweid, terrainType) :> AbstractComponent
