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
    inherit AbstractComponent(eid,Terrain)

    member _.Type = terrainType

    override this.NewWithEID eid = TerrainComponent(eid, this.Type) :> AbstractComponent

    new(terrainType:TerrainType) = TerrainComponent(0u, terrainType)
    
