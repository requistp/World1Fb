module VisionComponent
open LocationTypes

        
type VisionComponent = 
    { 
        EntityID : uint32
        Range : int
        RangeTemplate : LocationDataInt[]
        //ViewedMap : LocationDataInt[]     // All locations that entity has ever seen
        ViewableMap : LocationDataInt[]   // Locations that are visible taking into account occlusion, etc. (i.e. a subset of VisionMap)
        VisionMap : LocationDataInt[]     // Locations within range--regardless of being blocked/visible/etc.
    } 

    member me.Update (rangeUpdate:int option) (viewableMapUpdate:LocationDataInt[] option) (visionMapUpdate:LocationDataInt[] option) =
        {
            me with
                Range = if rangeUpdate.IsSome then rangeUpdate.Value else me.Range
                ViewableMap = if viewableMapUpdate.IsSome then viewableMapUpdate.Value else me.ViewableMap
                VisionMap = if visionMapUpdate.IsSome then visionMapUpdate.Value else me.VisionMap
        }

    