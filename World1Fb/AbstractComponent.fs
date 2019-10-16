module AbstractComponent


type ComponentTypes =
    | Eating
    | Food
    | Form
    | Controller
    | Movement
    | Terrain


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid
    
