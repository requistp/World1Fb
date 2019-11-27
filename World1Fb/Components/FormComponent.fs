module FormComponent
open CommonGenericFunctions
open LocationTypes
open System.Runtime.CompilerServices

//[<IsByRefLike; Struct>]
[<Struct>]
type FormComponent(id:ComponentID, eid:EntityID, born:RoundNumber, canSeePast:bool, isPassable:bool, location:LocationDataInt, name:string, symbol:char) = 
    //struct
        member _.ID = id
        member _.EntityID = eid
        member _.Born = born
        member _.CanSeePast = canSeePast
        member _.IsPassable = isPassable
        member _.Location = location
        member _.Name = name
        member _.Symbol = symbol

        //new(id:ComponentID, eid:EntityID, born:RoundNumber, canSeePast:bool, isPassable:bool, location:LocationDataInt, name:string, symbol:char ) =
        //    {
        //        ID = id    
        //    }
    //end
    
//let UpdateForm (f:FormComponent) (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
//    {
//        f with
//            IsPassable = if isPassableUpdate.IsSome then isPassableUpdate.Value else f.IsPassable
//            Location = if locationUpdate.IsSome then locationUpdate.Value else f.Location
//            Name = if nameUpdate.IsSome then nameUpdate.Value else f.Name
//            Symbol = if symbolUpdate.IsSome then symbolUpdate.Value else f.Symbol
//    }

