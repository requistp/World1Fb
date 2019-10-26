module AbstractComponent


type ComponentTypes =
    | Component_Controller 
    | Component_Eating     
    | Component_Food       
    | Component_Form       
    | Component_Movement   
    | Component_PlantGrowth
    | Component_Terrain
    with 
    static member AsArray = 
        [|
            Component_Controller 
            Component_Eating     
            Component_Food       
            Component_Form       
            Component_Movement   
            Component_PlantGrowth
            Component_Terrain    
        |]


[<AbstractClass>]
type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    member this.ComponentType = componentType
    member this.EntityID = eid

    abstract member Copy : uint32 -> AbstractComponent

    member this.Abstract = this :> AbstractComponent

