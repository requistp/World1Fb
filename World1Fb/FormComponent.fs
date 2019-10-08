module FormComponent
open AbstractComponent
open LocationTypes

[<Literal>]
let ComponentID_Form = 1uy

type FormComponent(isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(ComponentID_Form)
    member _.IsPassable = isPassable
    member _.Name = name
    member _.Symbol = symbol
    member _.Location = location

    
//type FormComponent_Change = {
//       IsPassable : bool option
//       Name : string option
//       Symbol : char option
//       Location : LocationDataInt option
//   }