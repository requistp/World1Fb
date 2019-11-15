module VisionSystem
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open FormComponent
open LocationTypes
open vision_Shadowcast
open SystemManager
open VisionComponent


type VisionSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    let handleFOV (form:FormComponent) (vision:VisionComponent) (visionMap:LocationDataInt[]) =
        let forms = 
            visionMap 
            |> Array.fold (fun (m:Map<LocationDataInt,FormComponent[]>) location -> 
                let cts = 
                    location
                    |> History.GetEntitiesAtLocationWithComponent enm.AgentEntities FormComponentID None
                    |> Array.Parallel.map (fun e -> e.ToForm)
                m.Add(location,cts)
                ) Map.empty
        ComputeVisibility form.Location visionMap forms vision.Range

    member private me.onLocationChanged round (ge:GameEventTypes) =
        let lc = ge.ToLocationChanged
        match lc.EntityID |> enm.TryGetComponent VisionComponentID with
        | None -> Ok (Some "No vision Component")
        | Some v ->
            let vision = v.ToVision
            let form = lc.Form.ToForm
            let visionMap = LocationsWithinRange2D form.Location vision.RangeTemplate
            let viewableMap = handleFOV form vision visionMap
            enm.ReplaceComponent (Vision (v.ToVision.Update round None (Some viewableMap) (Some visionMap)))
            Ok None

    override me.Initialize = 
        evm.RegisterListener me.Description Event_LocationChanged_ID (me.TrackTask me.onLocationChanged)
        base.SetToInitialized

    override me.Update round = 
        ()


