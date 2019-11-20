module PlantGrowthComponent
open CommonGenericFunctions
open ComponentEnums


type PlantGrowthComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        GrowsInTerrain : TerrainType[]
        RegrowRate : float
        ReproductionRate : float
        ReproductionRange : int
        ReproductionRequiredFoodQuantity : float 
    } 


