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
    
let UpdateForm (f:FormComponent) (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
    {
        f with
            IsPassable = if isPassableUpdate.IsSome then isPassableUpdate.Value else f.IsPassable
            Location = if locationUpdate.IsSome then locationUpdate.Value else f.Location
            Name = if nameUpdate.IsSome then nameUpdate.Value else f.Name
            Symbol = if symbolUpdate.IsSome then symbolUpdate.Value else f.Symbol
    }

