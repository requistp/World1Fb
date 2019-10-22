module PlantGrowthComponent
open AbstractComponent
open CalendarTimings
open TerrainComponent

type PlantGrowthComponent(eid:uint32, growsInTerrain:TerrainType[], regrowRate:float, reproductionLast:uint32, reproductionRate:float, reproductionRange:int) = 
    inherit AbstractComponent(eid,Component_PlantGrowth)

    member _.GrowsInTerrain = growsInTerrain
    member _.RegrowRate = regrowRate
    member _.ReproductionLast = reproductionLast
    member _.ReproductionRate = reproductionRate
    member _.ReproductionRange = reproductionRange
    
    member this.Update (reproducedLast:uint32) =
        PlantGrowthComponent(eid, growsInTerrain, regrowRate, reproducedLast, reproductionRate, reproductionRange)

    new (eid:uint32, growsInTerrain:TerrainType[], regrowRate:float, reproductionRate:float, reproductionRange:int) =
        PlantGrowthComponent(eid, growsInTerrain, regrowRate, 0u, reproductionRate, reproductionRange) 

    
    override this.Copy neweid = 
        PlantGrowthComponent(neweid, growsInTerrain, regrowRate, reproductionLast, reproductionRate, reproductionRange) :> AbstractComponent

