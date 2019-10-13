module AbstractComponent


type ComponentTypes =
     | Form
     | Controller
     | Movement
     | Terrain


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member _.ComponentType = componentType
    member _.EntityID = eid


[<AbstractClass>]
type AbstractComponentChange(componentType:ComponentTypes, eid:uint32) =
    member _.ComponentType = componentType
    member _.EntityID = eid

    abstract member AddChange : AbstractComponentChange -> AbstractComponentChange
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
    member this.Append (scl:SystemChangeLog) = 
        {
            ComponentChanges = scl.ComponentChanges |> Array.append this.ComponentChanges
            NewEntities = scl.NewEntities |> Array.append this.NewEntities
        }
    member this.Add_ComponentChange (c:AbstractComponentChange) =
        { this with ComponentChanges = Array.append this.ComponentChanges [|c|] }
    member this.Add_NewEntity (ne:AbstractComponent[]) =
        { this with NewEntities = Array.append this.NewEntities [|ne|] }
