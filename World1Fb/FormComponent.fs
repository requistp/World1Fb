module FormComponent
open LocationTypes

[<Literal>]
let ComponentID_Form = 1uy

type FormComponent = {
        IsPassable : bool
        Name : string
        Symbol : char
        Location : LocationDataInt
    }
    
type FormComponent_Change = {
       IsPassable : bool option
       Name : string option
       Symbol : char option
       Location : LocationDataInt option
   }