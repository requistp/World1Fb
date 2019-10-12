﻿module FormComponent
open AbstractComponent
open CommonGenericFunctions
open LocationTypes


type FormComponent(eid:uint32, isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(eid,Form)

    member _.IsPassable = isPassable
    member _.Name = name
    member _.Symbol = symbol
    member _.Location = location

    override this.NewWithEID eid = FormComponent(eid, this.IsPassable, this.Name, this.Symbol, this.Location) :> AbstractComponent

    new(isPassable:bool, name:string, symbol:char, location:LocationDataInt) = FormComponent(0u, isPassable, name, symbol, location)

    
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
        FormComponent(eid, Option.defaultValue c.IsPassable this.IsPassable, c.Name, Option.defaultValue c.Symbol this.Symbol, location.Add c.Location) :> AbstractComponent

    override this.AddChange (a:AbstractComponentChange) =
        let c = a :?> FormComponent_Change
        FormComponent_Change(eid, ResolveCombiningTwoOptions isPassable c.IsPassable, ResolveCombiningTwoOptions symbol c.Symbol, location.Add c.Location) :> AbstractComponentChange

        