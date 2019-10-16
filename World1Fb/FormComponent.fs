module FormComponent
open AbstractComponent
open LocationTypes


type FormComponent(eid:uint32, isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(eid,Form)

    member _.IsPassable = isPassable
    member _.Name = name
    member _.Symbol = symbol
    member _.Location = location

    