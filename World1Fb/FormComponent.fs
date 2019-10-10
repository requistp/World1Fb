﻿module FormComponent
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

//    member this.AddChange (c:FormComponent) =
//        FormComponent(Option.defaultValue c.IsPassable this.IsPassable, c.Name, Option.defaultValue c.Symbol this.Symbol, location.Add c.Location)

//    member this.AddChange (c:FormComponent_Change) =
//        FormComponent_Change(eid, ResolveCombiningTwoOptions isPassable c.IsPassable, ResolveCombiningTwoOptions symbol c.Symbol, location.Add c.Location)

    override this.AddChange (a:AbstractComponent) =
        let c = a :?> FormComponent
        FormComponent(Option.defaultValue c.IsPassable this.IsPassable, c.Name, Option.defaultValue c.Symbol this.Symbol, location.Add c.Location) :> AbstractComponent

    override this.AddChange (a:AbstractComponentChange) =
        let c = a :?> FormComponent_Change
        FormComponent_Change(eid, ResolveCombiningTwoOptions isPassable c.IsPassable, ResolveCombiningTwoOptions symbol c.Symbol, location.Add c.Location) :> AbstractComponentChange

        