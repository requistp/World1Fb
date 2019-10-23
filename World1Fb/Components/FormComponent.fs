module FormComponent
open AbstractComponent
open LocationTypes


type FormComponent(eid:uint32, isPassable:bool, name:string, symbol:char, location:LocationDataInt) = 
    inherit AbstractComponent(eid,Component_Form)

    member _.IsPassable = isPassable
    member _.Name = name
    member _.Symbol = symbol
    member _.Location = location

    member this.Update (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
        FormComponent(eid, 
            (if isPassableUpdate.IsSome then isPassableUpdate.Value else isPassable), 
            (if nameUpdate.IsSome then nameUpdate.Value else name), 
            (if symbolUpdate.IsSome then symbolUpdate.Value else symbol), 
            (if locationUpdate.IsSome then locationUpdate.Value else location)
            )

    override this.Copy neweid = 
        FormComponent(neweid, isPassable, name, symbol, location).Abstract


