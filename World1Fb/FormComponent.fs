module FormComponent
open AbstractComponent
open CommonGenericFunctions
open LocationTypes


type FormComponent(isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(Form)
    member _.IsPassable = isPassable
    member _.Name = name
    member _.Symbol = symbol
    member _.Location = location

    
type FormComponent_Change(eid:uint32, isPassable:bool option, symbol:char option, location:LocationDataInt) =
    inherit AbstractComponentChange(Form,eid)
    member _.IsPassable = isPassable
    member _.Symbol = symbol
    member _.Location = location

    member _.AddChange (c:FormComponent_Change) =
        FormComponent_Change(eid, ResolveAddingTwoOptions isPassable c.IsPassable, ResolveAddingTwoOptions symbol c.Symbol, location.Add c.Location)
