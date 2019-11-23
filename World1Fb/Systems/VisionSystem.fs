module VisionSystem
open Component
open ComponentEnums
open EntityExtensions
open EventManager
open EventTypes
open FormComponent
open LocationTypes
open vision_Shadowcast
open SystemManager
open VisionComponent
open EntityManager


type VisionSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    let handleFOV (form:FormComponent) (vision:VisionComponent) (visionMap:LocationDataInt[]) =
        //redo this so that I handle all entities visions at once during the round update
        let allForms = enm.GetLocationMap None
        let forms = 
            visionMap 
            |> Array.fold (fun (m:Map<LocationDataInt,FormComponent[]>) location -> 
                m.Add(location,allForms.Item(location))
                ) Map.empty
        ComputeVisibility form.Location visionMap forms vision.Range

    member private me.onLocationChanged round (LocationChanged form:GameEventTypes) =
        match EntityExt.TryGetComponent enm None VisionComponentID form.EntityID with
        | None -> Ok (Some "No vision Component")
        | Some (Vision vision) ->
            let visionMap = LocationsWithinRange2D form.Location vision.RangeTemplate
            let viewableMap = handleFOV form vision visionMap
            enm.UpdateComponent round (Vision (vision.Update round None (Some viewableMap) (Some visionMap)))
            Ok None

    override me.Initialize = 
        evm.RegisterListener me.Description Event_LocationChanged_ID (me.TrackTask me.onLocationChanged)
        base.SetToInitialized

    override me.Update round = 
        ()




(*
let handleFOV (form:FormComponent) (vision:VisionComponent) (visionMap:LocationDataInt[]) =
    let forms = 
        visionMap 
        |> Array.fold (fun (m:Map<LocationDataInt,FormComponent[]>) location -> 
            let cts = 
                location
                |> EntityExt.GetEntitiesAtLocationWithComponent enm None FormComponentID None
                |> Array.Parallel.map ToForm
            m.Add(location,cts)
            ) Map.empty
    ComputeVisibility form.Location visionMap forms vision.Range
    *)
