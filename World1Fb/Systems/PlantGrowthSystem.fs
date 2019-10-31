module PlantGrowthSystem
open Component
open AbstractSystem
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes
open GameManager
open LocationTypes

type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onCreateEntity (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity
        match e.Components |> Array.filter (fun c -> c.ComponentID = PlantGrowthData.ID ) with
        | [||] -> Ok (Some "No PlantGrowthComponent")
        | c -> let (PlantGrowth pd) = c.[0] 
               if pd.RegrowRate > 0.0 then evm.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantRegrowth,e.EntityID),uint32 PlantGrowthFrequency)))
               if pd.ReproductionRate > 0.0 then evm.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(PlantReproduce,e.EntityID),uint32 PlantReproductionFrequency)))
               Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pd.RegrowRate > 0.0) (pd.ReproductionRate > 0.0)))
    
    member private me.onReproduce (ge:EventData_Generic) =
        let pd = (enm.GetComponent PlantGrowthData.ID ge.EntityID).ToPlantGrowth
        let tryMakeNewPlant =            
            let r = random.NextDouble()
            match pd.ReproductionRate >= r with
            | false -> Error (sprintf "Failed: reproduction rate (%f<%f)" pd.ReproductionRate r)
            | true -> 
                let form = (enm.GetComponent FormData.ID ge.EntityID).ToForm
                let newLocation = form.Location.AddOffset pd.ReproductionRange pd.ReproductionRange 0 false true
                match newLocation.IsOnMap with
                | false -> Error (sprintf "Failed: location not on map:%s" (newLocation.ToString()))
                | true ->
                    let eids = enm.GetEntitiesAtLocation newLocation
                    match (eids |> enm.TryGetComponentForEntities PlantGrowthData.ID).Length with 
                    | x when x > 0 -> Error (sprintf "Failed: plant exists at location:%s" (newLocation.ToString()))
                    | _ -> 
                        match pd.GrowsInTerrain|>Array.contains (eids|>enm.TryGetComponentForEntities TerrainData.ID).[0].ToTerrain.Terrain with
                        | false -> Error "Failed: terrain is not suitable"
                        | true -> 
                            let fco = ge.EntityID |> enm.TryGetComponent FoodData.ID 
                            match fco.IsNone with
                            | true -> Ok (newLocation,r)
                            | false -> 
                                let (Food fd) = fco.Value
                                let pct = float fd.Quantity / float fd.QuantityMax
                                match pd.ReproductionRequiredFoodQuantity < pct with
                                | false -> Error (sprintf "Failed: food component quantity below requirement (%f<%f)" pct pd.ReproductionRequiredFoodQuantity)
                                | true -> Ok (newLocation,r)
        let makePlant_AdjustComponents (c:Component) (l:LocationDataInt) =
            match c with
            | Food d -> 
                Food (d.Update None (Some 1) None)
            | Form d -> 
                Form (d.Update None None None (Some l))
            | _ -> c        
        let makePlant (l:LocationDataInt) (r:float) = 
            let neweid = enm.GetNewID
            let newcts = 
                enm.CopyEntity ge.EntityID neweid
                |> Array.Parallel.map (fun c -> makePlant_AdjustComponents c l)
            evm.QueueEvent (EventData_CreateEntity(newcts))
            Ok (Some (sprintf "Passed reproduction (%f>%f). EntityID:%i. Location:%s" pd.ReproductionRate r neweid (l.ToString())))

        match tryMakeNewPlant with
        | Error s -> Error s
        | Ok (l,r) -> makePlant l r

    override me.Initialize = 
        evm.RegisterListener "PlantGrowthSystem" CreateEntity me.onCreateEntity
        evm.RegisterListener "PlantGrowthSystem" PlantReproduce me.onReproduce
        base.SetToInitialized

    override this.Update = 
        ()


