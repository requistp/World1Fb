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
        | ct -> let pgc = ct.[0] :?> PlantGrowthComponent
                if pgc.RegrowRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantRegrowth,e.EntityID),uint32 PlantGrowthFrequency)))
                if pgc.ReproductionRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantReproduce,e.EntityID),uint32 PlantReproductionFrequency)))
                Ok None
    
    member private this.onReproduce (next:NextEntityDictionary) (ge:EventData_Generic) =
        match next.Entities.ContainsKey(ge.EntityID) with
        | false -> Ok None
        | true ->
            let neweid = next.NewEntityID
            let newcts = next.Copy ge.EntityID neweid
            
            Ok None

    override this.Initialize = 
        game.EventManager.RegisterListener CreateEntity this.onCreateEntity
        game.EventManager.RegisterListener PlantReproduce this.onReproduce
        base.SetToInitialized

    override this.Update = 
        ()
