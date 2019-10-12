module TerrainComponent
open AbstractComponent
open LocationTypes


type TerrainType = 
    | Dirt 
    | Rock
    | Sand


type TerrainComponent(eid:uint32, terrainType:TerrainType, location:LocationDataInt) =
    inherit AbstractComponent(eid,Terrain)

    member _.Type = terrainType
    member _.Location = location  
    member _.IsPassable = 
        match terrainType with
        | Dirt | Sand -> true
        | Rock -> false
    member _.Symbol = 
        match terrainType with
        | Dirt -> '.'
        | Sand -> ','
        | Rock -> '#'

    override this.NewWithEID eid = TerrainComponent(eid, this.Type, this.Location) :> AbstractComponent

    new(terrainType:TerrainType, location:LocationDataInt) = TerrainComponent(0u, terrainType, location)
    
