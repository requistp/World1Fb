module PlantGrowthSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open CalendarTimings
open EntityExtensions
open EventManager
open EventTypes
open FoodComponent
open FormComponent
open LocationTypes
open PlantGrowthComponent
open SystemManager
open EntityManager


type PlantGrowthSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive)
  
    member private me.onComponentAdded round (ComponentAdded_PlantGrowth pgc:GameEventData) =
        if pgc.RegrowRate > 0.0 then evm.AddToSchedule { ScheduleType = RepeatIndefinitely; Frequency = PlantGrowthFrequency; GameEvent = PlantRegrowth pgc }
        if pgc.ReproductionRate > 0.0 then evm.AddToSchedule { ScheduleType = RepeatIndefinitely; Frequency = PlantReproductionFrequency; GameEvent = PlantReproduce pgc }
        Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pgc.RegrowRate > 0.0) (pgc.ReproductionRate > 0.0)))
  
    member private me.onReproduce round (PlantReproduce pgc:GameEventData) =
        let makePlant (l:LocationDataInt) = 
            let adjustComponents (c:Component) =
                match c with
                | Food d -> 
                    Food (UpdateFood d None (Some 1) None)
                | Form d -> 
                    Form (UpdateForm d None None None (Some l))
                | _ -> c          
            let newcts = 
                pgc.EntityID
                |> EntityExt.CopyEntity enm round
                |> Array.Parallel.map adjustComponents
            evm.RaiseEvent (CreateEntity newcts)
            Ok (Some (sprintf "New plant:%i. Location:%s" (GetComponentEntityID newcts.[0]).ToUint32 (l.ToString())))

        let tryMakeNewPlant = 
            let r = random.NextDouble()
            match pgc.ReproductionRate >= r with
            | false -> Error (sprintf "Failed: reproduction rate (%f<%f)" pgc.ReproductionRate r)
            | true -> 
                let newLocation = (EntityExt.GetLocation enm None pgc.EntityID).AddOffset pgc.ReproductionRange pgc.ReproductionRange 0 false true
                match newLocation.IsOnMap with
                | false -> Error (sprintf "Failed: location not on map:%s" (newLocation.ToString()))
                | true -> 
                    let eids = enm.GetEntityIDsAtLocation None newLocation
                    match (EntityExt.GetComponentForEntities enm None PlantGrowthComponent eids).Length with 
                    | x when x > 0 -> Error (sprintf "Failed: plant exists at location:%s" (newLocation.ToString()))
                    | _ -> 
                        match pgc.GrowsInTerrain|>Array.contains (ToTerrain (EntityExt.GetComponentForEntities enm None TerrainComponent eids).[0]).Terrain with
                        | false -> Error "Failed: terrain is not suitable"
                        | true -> 
                            match (EntityExt.TryGetComponent enm None FoodComponent pgc.EntityID) with
                            | None -> Ok newLocation
                            | Some (Food fd) -> 
                                let pct = float fd.Quantity / float fd.QuantityMax
                                match pgc.ReproductionRequiredFoodQuantity < pct with
                                | false -> Error (sprintf "Failed: food component quantity below requirement (%f<%f)" pct pgc.ReproductionRequiredFoodQuantity)
                                | true -> Ok newLocation
        match tryMakeNewPlant with
        | Error s -> Error s
        | Ok l -> makePlant l 

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_PlantGrowth (me.TrackTask me.onComponentAdded)
        evm.RegisterListener me.Description Event_PlantReproduce             (me.TrackTask me.onReproduce)
        base.SetToInitialized

    override me.Update round = 
        ()

