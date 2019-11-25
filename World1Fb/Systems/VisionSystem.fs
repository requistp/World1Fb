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

let UpdateViewableForAll (enm:EntityManager) round = 
    let allForms = enm.GetLocationMap 

    VisionComponent
    |> enm.GetComponentsOfType None
    |> Array.Parallel.map ToVision
    |> Array.Parallel.iter (fun v ->
        enm.UpdateComponent round (Vision (UpdateViewed v round (ComputeVisibility (EntityExt.GetLocation enm None v.EntityID) v.LocationsWithinRange allForms v.Range)))
        )

type VisionSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member private me.onLocationChanged round (LocationChanged form:GameEventData) =
        match EntityExt.TryGetComponent enm None VisionComponent form.EntityID with
        | None -> Ok (Some "No vision Component")
        | Some (Vision vision) ->
            enm.UpdateComponent round (Vision (UpdateVision vision None (Some (LocationsWithinRange2D form.Location vision.RangeTemplate))))
            Ok (Some "VisionMap updated")

    override me.Initialize = 
        evm.RegisterListener me.Description Event_LocationChanged (me.TrackTask me.onLocationChanged)
        base.SetToInitialized

    override me.Update round = 
        () 


    //let handleFOV (form:FormComponent) (vision:VisionComponent) =
    //    //redo this so that I handle all entities visions at once during the round update
    //    //let forms = 
    //    //    vision.LocationsWithinRange
    //    //    |> Array.fold (fun (m:Map<LocationDataInt,FormComponent[]>) location -> 
    //    //        m.Add(location,allForms.Item(location))
    //    //        ) Map.empty
    //    ComputeVisibility form.Location vision.LocationsWithinRange allForms vision.Range

    //    let addViewableLocations (newLocations:LocationDataInt[]) =
    //        newLocations
    //        |> Array.fold (fun (viewed:Map<LocationDataInt,RoundNumber>) location -> 
    //            match viewed.ContainsKey location with
    //            | true -> viewed.Remove(location).Add(location,round)
    //            | false -> viewed.Add(location,round)
    //            ) vision.ViewedMap
    
    //let updateViewable (v:VisionComponent) = 
    //    enm.UpdateComponent round (Vision (UpdateViewed v (ComputeVisibility2 round (EntityExt.GetLocation enm None v.EntityID) v.LocationsWithinRange allForms v.Range)))




(*

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

    member private me.onLocationChanged round (LocationChanged form:GameEventData) =
        match EntityExt.TryGetComponent enm None VisionComponent form.EntityID with
        | None -> Ok (Some "No vision Component")
        | Some (Vision vision) ->
            let visionMap = LocationsWithinRange2D form.Location vision.RangeTemplate
            let viewableMap = handleFOV form vision visionMap
            enm.UpdateComponent round (Vision (UpdateVision vision round None (Some viewableMap) (Some visionMap)))
            Ok None

    override me.Initialize = 
        evm.RegisterListener me.Description Event_LocationChanged (me.TrackTask me.onLocationChanged)
        base.SetToInitialized

    override me.Update round = 
        ()





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
