module FormComponent
open AbstractComponent
open LocationTypes


type FormComponent(isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(Form)
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


type MovementComponent_Change(eid:uint32, moveDirection:MovementDirection, x:int, y:int) =
    inherit AbstractComponent_Change(eid)
    member _.Movement = moveDirection
    member _.X = x
    member _.Y = y

type MovementComponent_ChangeSum(eid:uint32, x:int, y:int) =
    inherit AbstractComponent_ChangeSum(eid)
    member _.X = x
    member _.Y = y

