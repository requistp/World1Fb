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

    abstract member AddChange : AbstractComponentChange -> AbstractComponentChange
    abstract member AddChange : AbstractComponent -> AbstractComponent

//[<AbstractClass>]
//type AbstractEntityCreation(cts:AbstractComponent[]) =


//type EntityComponentChange = 
//    | ComponentChange of ComponentChangeType
//    | EntityAddition of ComponentType list
//    | EntityRemoval of uint32