module PlantGrowthSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open FoodComponent
open FormComponent
open PlantGrowthComponent
open EventTypes
open GameManager
open TerrainComponent
open LocationTypes

type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onCreateEntity (enm:EntityManager) (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity
        match e.Components |> Array.filter (fun ct -> ct.ComponentType = Component_PlantGrowth) with
        | [||] -> Ok (Some "No PlantGrowthComponent")
        | ct -> let pgc = ct.[0] :?> PlantGrowthComponent
                if pgc.RegrowRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantRegrowth,e.EntityID),uint32 PlantGrowthFrequency)))
                if pgc.ReproductionRate > 0.0 then game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantReproduce,e.EntityID),uint32 PlantReproductionFrequency)))
                Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pgc.RegrowRate > 0.0) (pgc.ReproductionRate > 0.0)))
    
    member private this.onReproduce (enm:EntityManager) (ge:EventData_Generic) =
        let pgc = enm.GetComponent<PlantGrowthComponent> ge.EntityID
        let tryMakeNewPlant =            
            let r = random.NextDouble()
            match pgc.ReproductionRate >= r with
            | false -> Error (sprintf "Failed reproduction (%f<%f)" pgc.ReproductionRate r)
            | true -> 
                let form = enm.GetComponent<FormComponent> ge.EntityID
                let newLocation = form.Location.AddOffset pgc.ReproductionRange pgc.ReproductionRange 0 false true
                match newLocation.IsOnMap with
                | false -> Error "Cannot reproduce: location not on map"
                | true ->
                    let eids = enm.GetEntitiesAtLocation newLocation
                    let plantexists = not (eids |> enm.TryGetComponentForEntities<PlantGrowthComponent> |> Array.isEmpty)
                    match not plantexists with 
                    | false -> Error "Cannot reproduce: plant exists at location"
                    | true -> 
                        let terrainissuitable = pgc.GrowsInTerrain |> Array.exists (fun t -> (eids |> enm.TryGetComponentForEntities<TerrainComponent>) |> Array.exists (fun t2 -> t2.Terrain = t))
                        match terrainissuitable with
                        | false -> Error "Cannot reproduce: terrain is not suitable"
                        | true -> 
                            let foodo = enm.TryGetComponent<FoodComponent> ge.EntityID
                            let pct = float foodo.Value.Quantity / float foodo.Value.QuantityMax
                            match foodo.IsNone || pgc.ReproductionRequiredFoodQuantity < pct with
                            | false -> Error (sprintf "Cannot reproduce: food component quantity below requirement (%f<%f)" pct pgc.ReproductionRequiredFoodQuantity)
                            | true -> Ok (newLocation,r)
        let makePlant_AdjustComponents (ct:AbstractComponent) (l:LocationDataInt) =
            match ct.ComponentType with
            | Component_Food -> 
                ((ct:?>FoodComponent).Update None (Some 1) None).Abstract
            | Component_Form -> 
                ((ct:?>FormComponent).Update None None None (Some l)).Abstract
            | _ -> ct        
        let makePlant (l:LocationDataInt) (r:float) = 
            let neweid = enm.GetNewID
            let newcts = 
                enm.CopyEntity ge.EntityID neweid
                |> Array.map (fun ct -> makePlant_AdjustComponents ct l)
            game.EventManager.QueueEvent (EventData_CreateEntity(neweid,newcts))
            Ok (Some (sprintf "Passed reproduction (%f>%f). New EntityID:%i" pgc.ReproductionRate r neweid))

        match tryMakeNewPlant with
        | Error s -> Error s
        | Ok (l,r) -> makePlant l r

    override this.Initialize = 
        //game.EventManager.RegisterListener "PlantGrowthSystem" CreateEntity this.onCreateEntity
        game.EventManager.RegisterListener "PlantGrowthSystem" PlantReproduce this.onReproduce
        base.SetToInitialized

    override this.Update = 
        ()


