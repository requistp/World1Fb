module PlantGrowthComponent
open AbstractComponent
open CalendarTimings

type PlantGrowthComponent(eid:uint32, regrowRate:float, lastReproduced:int option, reproductionFrequency:int, reproductionRange:int, timer:ComponentExecutionTimer) = 
    inherit AbstractComponent(eid,Component_PlantGrowth)

    static member Type = Component_PlantGrowth
    static member Frequency = 10 // roundsPerDay // If I change this, I need to change the regrowRate because 100% of that is applied per this update

    member _.LastReproduced = lastReproduced
    member _.RegrowRate = regrowRate
    member _.ReproductionFrequency = reproductionFrequency
    member _.ReproductionRange = reproductionRange
    member _.Timer = timer
    
    member this.Update (newLastReproduced:int option) =
        PlantGrowthComponent(eid, regrowRate, newLastReproduced, reproductionFrequency, reproductionRange, timer)

    new (eid:uint32, regrowRate:float, reproductionFrequency:int, reproductionRange:int) =
        PlantGrowthComponent(eid, regrowRate, None, reproductionFrequency, reproductionRange, ComponentExecutionTimer(PlantGrowthComponent.Frequency))


