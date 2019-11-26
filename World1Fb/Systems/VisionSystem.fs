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
    |> enm.GetComponentsOfType
    |> Array.Parallel.map ToVision
    |> Array.Parallel.iter (fun vision ->
        let visibleLocations = 
            match vision.VisionCalculationType with
            | Basic_Cheating -> ComputeVisibility_Basic vision.LocationsWithinRange allForms
            | Shadowcast1 -> ComputeVisibility_Shadowcast1 (EntityExt.GetLocation enm vision.EntityID) vision.LocationsWithinRange allForms vision.Range
        enm.UpdateComponent (Vision (UpdateViewed vision visibleLocations))
        )

type VisionSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member private me.onLocationChanged round (LocationChanged form:GameEventData) =
        match EntityExt.TryGetComponent enm VisionComponent form.EntityID with
        | None -> Ok (Some "No vision Component")
        | Some (Vision vision) ->
            enm.UpdateComponent (Vision (UpdateVision vision None (Some (LocationsWithinRange2D form.Location vision.RangeTemplate))))
            Ok (Some "VisionMap updated")

    override me.Initialize = 
        evm.RegisterListener me.Description Event_LocationChanged (me.TrackTask me.onLocationChanged)
        base.SetToInitialized

    override me.Update round = 
        () 




(*
let viewedHistory = 
    visibleLocations
    |> Map.fold (fun (m:Map<LocationDataInt,FormComponent[]>) l fs -> 
        let newFS =
            match (visibleLocations.ContainsKey l) with
            | true -> visibleLocations.Item l
            | false -> fs |> Array.filter (fun f -> not (fids |> Array.contains f.ID))
        m.Add(l,newFS)
    ) vision.ViewedHistory

//fs 
//|> Array.filter (fun (f:FormComponent) -> 
//    let visibleIDs = 
//        match (visibleLocations.ContainsKey l) with
//        | false -> [||]
//        | true -> visibleLocations.Item(l) |> Array.map (fun f -> f.ID)
//    not (visibleIDs |> Array.contains f.ID))

let viewedHistory = 
    match vision.ViewedHistory.IsEmpty with
    | true -> visibleLocations
    | false -> vision.ViewedHistory
    |> Map.fold (fun (m:Map<LocationDataInt,FormComponent[]>) l fs -> 
        let newFS =
            match  (visibleLocations.ContainsKey l) with // if in visibile locations, it will be handled next
            | true -> visibleLocations.Item l
            | false -> //fs |> Array.filter (fun (f:FormComponent) -> not (visibleIDs |> Array.contains f.ID))
                fs 
                |> Array.filter (fun (f:FormComponent) -> 
                    let visibleIDs = 
                        match (visibleLocations.ContainsKey l) with
                        | false -> [||]
                        | true -> visibleLocations.Item(l) |> Array.map (fun f -> f.ID)
                    not (visibleIDs |> Array.contains f.ID))
        m.Add(l,newFS)
    ) vision.ViewedHistory
*)



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
