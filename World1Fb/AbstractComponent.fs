module AbstractComponent


type ComponentTypes =
    | Comp_Eating
    | Comp_Food
    | Comp_Form
    | Comp_Controller
    | Comp_Movement
    | Comp_Terrain


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid
    