module VisionComponent
open CommonGenericFunctions
open LocationTypes

        
type VisionComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        Range : int
        RangeTemplate : LocationDataInt[]
        ViewedHistory : Map<LocationDataInt,RoundNumber> // All locations that entity has ever seen, and when
        VisibleLocations : LocationDataInt[]             // Locations that are visible taking into account occlusion, etc. (i.e. a subset of VisionMap)
        LocationsWithinRange : LocationDataInt[]         // Locations within range--regardless of being blocked/visible/etc.
    }

let UpdateVision (vision:VisionComponent) (rangeUpdate:int option) (locationsWithinRangeUpdate:LocationDataInt[] option) =
    {
        vision with
            Range = if rangeUpdate.IsSome then rangeUpdate.Value else vision.Range
            RangeTemplate = if rangeUpdate.IsSome then RangeTemplate2D rangeUpdate.Value else vision.RangeTemplate
            LocationsWithinRange = if locationsWithinRangeUpdate.IsSome then locationsWithinRangeUpdate.Value else vision.LocationsWithinRange
    } 

let UpdateViewed (vision:VisionComponent) (round:RoundNumber) (visibleLocations:LocationDataInt[]) =
    {
        vision with
            VisibleLocations = visibleLocations
            ViewedHistory = 
                visibleLocations 
                |> Array.fold (fun (m:Map<LocationDataInt,RoundNumber>) l -> 
                    match m.ContainsKey l with
                    | false -> m.Add(l,round)
                    | true -> m.Remove(l).Add(l,round)
                    ) vision.ViewedHistory
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