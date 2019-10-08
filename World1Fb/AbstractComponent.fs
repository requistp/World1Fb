module AbstractComponent

[<AbstractClass>]
type AbstractComponent_Change(eid:uint32) =
    member _.EntityID = eid

[<AbstractClass>]
type AbstractComponent_ChangeSum(eid:uint32) = 
    member _.EntityID = eid
