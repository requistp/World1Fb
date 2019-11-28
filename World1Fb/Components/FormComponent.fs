module FormComponent
open CommonGenericFunctions
open LocationTypes


type FormComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        Born : RoundNumber
        CanSeePast : bool
        IsPassable : bool
        Location : LocationDataInt
        Name : string
        Symbol : char 
    }
   
