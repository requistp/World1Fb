module PlantGrowthSystem
open Component
open ComponentEnums
open CalendarTimings
open CommonGenericFunctions
open EventTypes
open GameManager
open LocationTypes
open SystemManager


type PlantGrowthSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onComponentAdded (ge:GameEventTypes) =
        let e = ge.ToComponentAddedPlantGrowth
        let pd = e.Component.ToPlantGrowth
        if pd.RegrowRate > 0.0 then evm.ScheduleEvent (ScheduleEvent ({ Schedule=RepeatIndefinitely; Frequency=uint32 PlantGrowthFrequency }, PlantRegrowth { EntityID=e.EntityID }))
        if pd.ReproductionRate > 0.0 then evm.ScheduleEvent (ScheduleEvent ({ Schedule=RepeatIndefinitely; Frequency=uint32 PlantReproductionFrequency }, PlantReproduce { EntityID=e.EntityID }))
        Ok (Some (sprintf "Queued Regrow to Schedule:%b. Queued Repopulate to Schedule:%b" (pd.RegrowRate > 0.0) (pd.ReproductionRate > 0.0)))
    
    member private me.onReproduce (ge:GameEventTypes) =
        let e = ge.ToPlantReproduce
        let pd = (enm.GetComponent PlantGrowthComponentID e.EntityID).ToPlantGrowth
        let tryMakeNewPlant =            
            let r = random.NextDouble()
            match pd.ReproductionRate >= r with
            | false -> Error (sprintf "Failed: reproduction rate (%f<%f)" pd.ReproductionRate r)
            | true -> 
                let form = (enm.GetComponent FormComponentID e.EntityID).ToForm
                let newLocation = form.Location.AddOffset pd.ReproductionRange pd.ReproductionRange 0 false true
                match newLocation.IsOnMap with
                | false -> Error (sprintf "Failed: location not on map:%s" (newLocation.ToString()))
                | true ->
                    let eids = enm.GetEntitiesAtLocation newLocation
                    match (eids |> enm.TryGetComponentForEntities PlantGrowthComponentID).Length with 
                    | x when x > 0 -> Error (sprintf "Failed: plant exists at location:%s" (newLocation.ToString()))
                    | _ -> 
                        match pd.GrowsInTerrain|>Array.contains (eids|>enm.TryGetComponentForEntities TerrainComponentID).[0].ToTerrain.Terrain with
                        | false -> Error "Failed: terrain is not suitable"
                        | true -> 
                            let fco = e.EntityID |> enm.TryGetComponent FoodComponentID 
                            match fco.IsNone with
                            | true -> Ok (newLocation,r)
                            | false ->
                                let fd = fco.Value.ToFood
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
            let newcts = 
                enm.CopyEntity e.EntityID 
                |> Array.Parallel.map (fun c -> makePlant_AdjustComponents c l)
            evm.ExecuteEvent (CreateEntity { Components = newcts })
            Ok (Some (sprintf "Passed reproduction (%f>%f). EntityID:%i. Location:%s" pd.ReproductionRate r (newcts.[0].EntityID) (l.ToString())))

        match tryMakeNewPlant with
        | Error s -> Error s
        | Ok (l,r) -> makePlant l r

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ComponentAdded_PlantGrowth.ID me.onComponentAdded
        evm.RegisterListener me.ToString Event_PlantReproduce.ID             me.onReproduce
        base.SetToInitialized

    override _.ToString = "PlantGrowthSystem"

    override me.Update = 
        ()


