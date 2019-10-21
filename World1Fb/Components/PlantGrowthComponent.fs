module PlantGrowthComponent
open AbstractComponent
open CalendarTimings

type PlantGrowthComponent(eid:uint32, regrowRate:float, reproductionLast:uint32, reproductionRate:int, reproductionRange:int) = 
    inherit AbstractComponent(eid,Component_PlantGrowth)
    static member Type = Component_PlantGrowth

    member _.RegrowRate = regrowRate
    member _.ReproductionLast = reproductionLast
    member _.ReproductionRate = reproductionRate
    member _.ReproductionRange = reproductionRange
    
    member this.Update (reproducedLast:uint32) =
        PlantGrowthComponent(eid, regrowRate, reproducedLast, reproductionRate, reproductionRange)

    new (eid:uint32, regrowRate:float, reproductionFrequency:int, reproductionRange:int) =
        PlantGrowthComponent(eid, regrowRate, 0u, reproductionFrequency, reproductionRange) 


