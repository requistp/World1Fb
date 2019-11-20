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
    member me.Update (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
        {
            me with
                IsPassable = if isPassableUpdate.IsSome then isPassableUpdate.Value else me.IsPassable
                Location = if locationUpdate.IsSome then locationUpdate.Value else me.Location
                Name = if nameUpdate.IsSome then nameUpdate.Value else me.Name
                Symbol = if symbolUpdate.IsSome then symbolUpdate.Value else me.Symbol
        }
