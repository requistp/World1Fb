module TerrainComponent
open CommonGenericFunctions
open ComponentEnums


type TerrainComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        Terrain : TerrainType 
    } 
        
        