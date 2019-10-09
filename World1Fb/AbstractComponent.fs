module AbstractComponent


type ComponentTypes =
     | Form
     | Controller
     | Movement
     | Terrain


[<AbstractClass>]
type AbstractComponent(componentType:ComponentTypes) =
    member _.ComponentType = componentType


[<AbstractClass>]
type AbstractComponentChange(componentType:ComponentTypes, eid:uint32) =
    member _.ComponentType = componentType
    member _.EntityID = eid


//[<AbstractClass>]
//type AbstractComponent_ChangeSum(eid:uint32) = 
//    member _.EntityID = eid


//[<AbstractClass>]
//type EntityComponentChange() =


//type EntityComponentChange = 
//    | ComponentChange of ComponentChangeType
//    | EntityAddition of ComponentType list
//    | EntityRemoval of uint32