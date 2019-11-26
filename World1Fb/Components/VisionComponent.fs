module VisionComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes

type VisionCalculationTypes =
    | Basic_Cheating
    | Shadowcast1


type VisionComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        LocationsWithinRange : LocationDataInt[]                // Locations within range--regardless of being blocked/visible/etc.
        Range : int
        RangeTemplate : LocationDataInt[]
        VisionCalculationType : VisionCalculationTypes
        ViewedHistory : Map<LocationDataInt,FormComponent[]>    // All locations that entity has ever seen, and when
        VisibleLocations : Map<LocationDataInt,FormComponent[]> // Locations that are visible taking into account occlusion, etc. (i.e. a subset of VisionMap)
    }

let UpdateVision (vision:VisionComponent) (rangeUpdate:int option) (locationsWithinRangeUpdate:LocationDataInt[] option) =
    {
        vision with
            Range = if rangeUpdate.IsSome then rangeUpdate.Value else vision.Range
            RangeTemplate = if rangeUpdate.IsSome then RangeTemplate2D rangeUpdate.Value else vision.RangeTemplate
            LocationsWithinRange = if locationsWithinRangeUpdate.IsSome then locationsWithinRangeUpdate.Value else vision.LocationsWithinRange
    } 

let UpdateViewed (vision:VisionComponent) (visibleLocations:Map<LocationDataInt,FormComponent[]>) = 
    let fids =
        visibleLocations
        |> Map.toArray
        |> Array.collect snd
        |> Array.map (fun f -> f.ID)

    let viewedHistory = 
        vision.ViewedHistory
        |> Map.fold (fun (m:Map<LocationDataInt,FormComponent[]>) l fs -> 
            let newFS =
                match (m.ContainsKey l) with
                | true -> m.Item l
                | false -> fs |> Array.filter (fun f -> not (fids |> Array.contains f.ID))
            m.Add(l,newFS)
        ) visibleLocations
    {
        vision with
            VisibleLocations = visibleLocations
            ViewedHistory = viewedHistory
    }



    (*
    
    
    type VisionComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        Range : int
        RangeTemplate : LocationDataInt[]
        ViewedMap : Map<LocationDataInt,RoundNumber> // All locations that entity has ever seen, and when
        //ViewedMap2 : Map<LocationDataInt,RoundNumber*ComponentID[]> // All locations that entity has ever seen, and when, with the FormComponentID that was there
        ViewableMap : LocationDataInt[]              // Locations that are visible taking into account occlusion, etc. (i.e. a subset of VisionMap)
        VisionMap : LocationDataInt[]                // Locations within range--regardless of being blocked/visible/etc.
    }

let UpdateVision (vision:VisionComponent) round (rangeUpdate:int option) (viewableMapUpdate:LocationDataInt[] option) (visionMapUpdate:LocationDataInt[] option) =
    let addViewableLocations (newLocations:LocationDataInt[]) =
        newLocations
        |> Array.fold (fun (viewed:Map<LocationDataInt,RoundNumber>) location -> 
            match viewed.ContainsKey location with
            | true -> viewed.Remove(location).Add(location,round)
            | false -> viewed.Add(location,round)
            ) vision.ViewedMap
    {
        vision with
            Range = if rangeUpdate.IsSome then rangeUpdate.Value else vision.Range
            ViewableMap = if viewableMapUpdate.IsSome then viewableMapUpdate.Value else vision.ViewableMap
            VisionMap = if visionMapUpdate.IsSome then visionMapUpdate.Value else vision.VisionMap
            ViewedMap = 
                match viewableMapUpdate with
                | None -> vision.ViewedMap
                | Some v -> addViewableLocations v
    } 
    
    
    *)