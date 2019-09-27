module FormComponent
open LocationTypes

[<Literal>]
let ComponentID_Form = 1uy

[<Struct>]
type FormComponent(isPassable:bool, name:string, symbol:char, location:LocationDataInt) =
    member this.IsPassable = isPassable
    member this.Name = name
    member this.Symbol = symbol
    member this.Location = location

[<Struct>]
type FormComponent_Change(isPassable:bool option, name:string option, symbol:char option, location:LocationDataInt option) =
    member this.IsPassable = isPassable
    member this.Name = name
    member this.Symbol = symbol
    member this.Location = location
