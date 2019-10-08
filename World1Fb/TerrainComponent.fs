module TerrainComponent
open AbstractComponent
open LocationTypes


type TerrainType = 
    | Dirt 
    | Rock
    | Sand


type TerrainComponent(terrainType:TerrainType, location:LocationDataInt) =
    inherit AbstractComponent(Terrain)
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


//type TerrainComponent_Change = {
//    Type : TerrainType option
//    }
