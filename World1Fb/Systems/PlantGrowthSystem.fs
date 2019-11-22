module PlantGrowthSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open CalendarTimings
open EntityExtensions
open EventManager
open EventTypes
open LocationTypes
open SystemManager
open EntityManager


type PlantGrowthSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive)
  
    member private me.onComponentAdded round (ge:GameEventTypes) =
        let (ComponentAdded_PlantGrowth e) = ge
        let (PlantGrowth pd) = e.Component
        if pd.RegrowRate > 0.0 then evm.AddToSchedule (ScheduleEvent ({ Schedule = RepeatIndefinitely; Frequency = PlantGrowthFrequency }, PlantRegrowth { EntityID = e.EntityID }))
        if pd.ReproductionRate > 0.0 then evm.AddToSchedule (ScheduleEvent ({ Schedule = RepeatIndefinitely; Frequency = PlantReproductionFrequency }, PlantReproduce { EntityID = e.EntityID }))
        Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pd.RegrowRate > 0.0) (pd.ReproductionRate > 0.0)))
  
    member private me.onReproduce round (ge:GameEventTypes) =
        let (PlantReproduce e) = ge
        let (PlantGrowth pd) = enm.GetComponent None PlantGrowthComponentID e.EntityID

        let makePlant momID (l:LocationDataInt) = 
            let adjustComponents (c:Component) =
                match c with
                | Food d -> 
                    Food (d.Update None (Some 1) None)
                | Form d -> 
                    Form (d.Update None None None (Some l))
                | _ -> c          
            let newcts = 
                momID
                |> EntityExt.CopyEntity enm round
                |> Array.Parallel.map adjustComponents
            evm.RaiseEvent (CreateEntity { Components = newcts })
            Ok (Some (sprintf "New plant:%i. Location:%s" (newcts.[0].EntityID).ToUint32 (l.ToString())))

        let tryMakeNewPlant = 
            let r = random.NextDouble()
            match pd.ReproductionRate >= r with
            | false -> Error (sprintf "Failed: reproduction rate (%f<%f)" pd.ReproductionRate r)
            | true -> 
                let newLocation = (EntityExt.GetLocation enm None e.EntityID).AddOffset pd.ReproductionRange pd.ReproductionRange 0 false true
                match newLocation.IsOnMap with
                | false -> Error (sprintf "Failed: location not on map:%s" (newLocation.ToString()))
                | true -> 
                    let eids = enm.GetEntityIDsAtLocation None newLocation
                    match (EntityExt.GetComponentForEntities enm None PlantGrowthComponentID eids).Length with 
                    | x when x > 0 -> Error (sprintf "Failed: plant exists at location:%s" (newLocation.ToString()))
                    | _ -> 
                        match pd.GrowsInTerrain|>Array.contains (EntityExt.GetComponentForEntities enm None TerrainComponentID eids).[0].ToTerrain.Terrain with
                        | false -> Error "Failed: terrain is not suitable"
                        | true -> 
                            match (EntityExt.TryGetComponent enm None FoodComponentID e.EntityID) with
                            | None -> Ok newLocation
                            | Some (Food fd) -> 
                                let pct = float fd.Quantity / float fd.QuantityMax
                                match pd.ReproductionRequiredFoodQuantity < pct with
                                | false -> Error (sprintf "Failed: food component quantity below requirement (%f<%f)" pct pd.ReproductionRequiredFoodQuantity)
                                | true -> Ok newLocation
        match tryMakeNewPlant with
        | Error s -> Error s
        | Ok l -> makePlant e.EntityID l 

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_PlantGrowth_ID (me.TrackTask me.onComponentAdded)
        evm.RegisterListener me.Description Event_PlantReproduce_ID             (me.TrackTask me.onReproduce)
        base.SetToInitialized

    override me.Update round = 
        ()

