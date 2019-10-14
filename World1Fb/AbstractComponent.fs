module AbstractComponent


type ComponentTypes =
     | Form
     | Controller
     | Movement
     | Terrain


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid
    

[<AbstractClass>]
type AbstractComponentChange(componentType:ComponentTypes, eid:uint32, invalid:string option) =
    member this.ComponentType = componentType
    member this.EntityID = eid
    member this.Invalid = invalid

    //abstract member AddChange : AbstractComponentChange -> AbstractComponentChange
    abstract member AddChange : AbstractComponent -> AbstractComponent


type SystemChangeLog = 
    {
        ComponentChanges : AbstractComponentChange[]
        NewEntities : AbstractComponent[][]
    } with 
    static member empty = 
        { 
            ComponentChanges = Array.empty
            NewEntities = Array.empty
        }

