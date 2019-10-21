module PlantGrowthSystem
open AbstractSystem
open CalendarTimings
open EntityDictionary
open PlantGrowthComponent
open GameEvents
open GameManager


type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onCreateEntity (next:NextEntityDictionary) (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity 
        match e.Components |> Array.filter (fun ct -> ct.ComponentType = PlantGrowthComponent.Type) with
        | [||] -> Ok None
        | ct -> game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantRegrowth,e.EntityID),uint32 PlantGrowthFrequency)))
                Ok None
       
    override this.Initialize = 
        game.EventManager.RegisterListener CreateEntity this.onCreateEntity
        base.SetToInitialized

    override this.Update = 
        ()
