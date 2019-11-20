module VisionComponent
open CommonGenericFunctions
open LocationTypes

        
type VisionComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        Range : int
        RangeTemplate : LocationDataInt[]
        ViewedMap : Map<LocationDataInt,uint32> // All locations that entity has ever seen
        ViewableMap : LocationDataInt[]         // Locations that are visible taking into account occlusion, etc. (i.e. a subset of VisionMap)
        VisionMap : LocationDataInt[]           // Locations within range--regardless of being blocked/visible/etc.
    }
    member me.Update round (rangeUpdate:int option) (viewableMapUpdate:LocationDataInt[] option) (visionMapUpdate:LocationDataInt[] option) =
        let addViewableLocations (newLocations:LocationDataInt[]) =
            newLocations
            |> Array.fold (fun (viewed:Map<LocationDataInt,uint32>) location -> 
                match viewed.ContainsKey location with
                | true -> viewed.Remove(location).Add(location,round)
                | false -> viewed.Add(location,round)
                ) me.ViewedMap
        {
            me with
                Range = if rangeUpdate.IsSome then rangeUpdate.Value else me.Range
                ViewableMap = if viewableMapUpdate.IsSome then viewableMapUpdate.Value else me.ViewableMap
                VisionMap = if visionMapUpdate.IsSome then visionMapUpdate.Value else me.VisionMap
                ViewedMap = 
                    match viewableMapUpdate with
                    | None -> me.ViewedMap
                    | Some v -> addViewableLocations v
        } 


    