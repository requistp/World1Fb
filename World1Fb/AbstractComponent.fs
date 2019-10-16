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
    

//[<AbstractClass>]
//type AbstractComponentChange(componentType:ComponentTypes, eid:uint32, invalid:string option) =
//    member this.ComponentType = componentType
//    member this.EntityID = eid
//    member this.Invalid = invalid

//    abstract member AddChange : AbstractComponent -> AbstractComponent
//    abstract member Invalidate : string -> AbstractComponentChange




