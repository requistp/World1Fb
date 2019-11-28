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



