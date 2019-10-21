module PlantGrowthSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityDictionary
open FoodComponent
open FormComponent
open PlantGrowthComponent
open EventTypes
open GameManager


type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onCreateEntity (next:NextEntityDictionary) (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity
        
        match e.Components |> Array.filter (fun ct -> ct.ComponentType = Component_PlantGrowth) with
        | [||] -> Ok None
        | ct -> let pgc = ct.[0] :?> PlantGrowthComponent
                if pgc.RegrowRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantRegrowth,e.EntityID),uint32 PlantGrowthFrequency)))
                if pgc.ReproductionRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantReproduce,e.EntityID),uint32 PlantReproductionFrequency)))
                Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pgc.RegrowRate > 0.0) (pgc.ReproductionRate > 0.0)))
    
    member private this.onReproduce (next:NextEntityDictionary) (ge:EventData_Generic) =
        let copyAndReplaceComponents (ct:AbstractComponent) (pgc:PlantGrowthComponent) =
            match ct.ComponentType with
            | Component_Food -> (ct :?> FoodComponent).Update None (Some 1) None :> AbstractComponent
            | Component_Form -> 
                let f = ct :?> FormComponent
                // have to check for food at new location
                // alos have to find terrain that is dirt, or maybe what the plant grows in
                f.Update None None None (Some (f.Location.Add (LocationTypes.LocationDataInt.Offset pgc.ReproductionRange pgc.ReproductionRange 0 false true))) :> AbstractComponent
            | _ -> ct

        match next.Entities.ContainsKey(ge.EntityID) with
        | false -> Ok (Some "Entity not in dictionary")
        | true ->
            let pgc = next.GetComponent<PlantGrowthComponent> ge.EntityID
            let r = random.NextDouble()
            match pgc.ReproductionRate >= r with
            | false -> Ok (Some (sprintf "Failed reproduction (%f<%f)" pgc.ReproductionRate r))
            | true -> 
                let neweid = next.NewEntityID
                let newcts = 
                    game.EntityManager.Copy ge.EntityID neweid
                    |> Array.map (fun ct -> copyAndReplaceComponents ct)
                game.EventManager.QueueEvent (EventData_CreateEntity(neweid,newcts))
                Ok (Some (sprintf "Passed reproduction (%f>%f)" pgc.ReproductionRate r))

    override this.Initialize = 
        game.EventManager.RegisterListener CreateEntity this.onCreateEntity
        game.EventManager.RegisterListener PlantReproduce this.onReproduce
        base.SetToInitialized

    override this.Update = 
        ()
