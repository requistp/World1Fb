module AbstractComponent


[<AbstractClass>]
type AbstractComponent(componentID:byte) =
    member _.ComponentID = componentID


[<AbstractClass>]
type AbstractComponent_Change(eid:uint32) =
    member _.EntityID = eid


[<AbstractClass>]
type AbstractComponent_ChangeSum(eid:uint32) = 
    member _.EntityID = eid


//[<AbstractClass>]
//type EntityComponentChange() =


//type EntityComponentChange = 
//    | ComponentChange of ComponentChangeType
//    | EntityAddition of ComponentType list
//    | EntityRemoval of uint32