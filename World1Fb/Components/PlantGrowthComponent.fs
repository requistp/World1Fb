module PlantGrowthComponent
open CommonGenericFunctions
open ComponentEnums

[<Struct>]
type PlantGrowthComponent (id:ComponentID, eid:EntityID, growsInTerrain:TerrainType[], regrowRate:float, reproductionRate:float, reproductionRange:int, reproductionRequiredFoodQuantity:float) = 
    member _.ID = id
    member _.EntityID = eid
    member _.GrowsInTerrain = growsInTerrain
    member _.RegrowRate = regrowRate
    member _.ReproductionRate = reproductionRate
    member _.ReproductionRange = reproductionRange
    member _.ReproductionRequiredFoodQuantity = reproductionRequiredFoodQuantity


