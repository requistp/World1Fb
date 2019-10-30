module AbstractComponent
open LocationTypes

type FoodClassifications =
    | Meat
    | Plant
type FoodTypes =
    | Food_Carrot
    | Food_Grass
    | Food_Meat_Rabbit
    member this.Calories = // This is calories per Quantity
        match this with 
        | Food_Carrot -> 2
        | Food_Grass -> 1
        | Food_Meat_Rabbit -> 3
    member this.Classification =
        match this with
        | Food_Carrot | Food_Grass -> Plant
        | Food_Meat_Rabbit -> Meat
    member this.KillOnAllEaten =
        match this with 
        | Food_Grass | Food_Meat_Rabbit -> false
        | Food_Carrot -> true
    member this.Symbol =
        match this.Classification with
        | Plant -> Some '!'
        | Meat -> None

type TerrainType = 
    | Dirt 
    | Rock
    | Sand
    member this.IsPassable = 
        match this with
        | Dirt | Sand -> true
        | Rock -> false
    member this.Symbol = 
        match this with
        | Dirt -> '.'
        | Sand -> ','
        | Rock -> '#'

//type FoodData = { FoodType:FoodTypes; Quantity:int; QuantityMax:int }
type FormData = { EntityID:uint32; IsPassable:bool; Location:LocationDataInt; Name:string; Symbol:char }
    //with 
    //member me.Update (isPassableUpdate:bool option) (nameUpdate:string option) (symbolUpdate:char option) (locationUpdate:LocationDataInt option) =
    //    {
    //        EntityID = me.EntityID
    //        IsPassable = if isPassableUpdate.IsSome then isPassableUpdate.Value else me.IsPassable
    //        Location = if locationUpdate.IsSome then locationUpdate.Value else me.Location
    //        Name = if nameUpdate.IsSome then nameUpdate.Value else me.Name
    //        Symbol = if symbolUpdate.IsSome then symbolUpdate.Value else me.Symbol
    //    }

type TerrainData = { EntityID:uint32; Terrain:TerrainType }

type Component = 
    //| Controller of entityID:uint32
//    | Eating of entityID:uint32
    //| Food of entityID:uint32
    | Form of FormData //entityID:uint32 * FormData //{| EntityID:uint32; IsPassable:bool; Location:LocationDataInt; Name:string; Symbol:char |}
//    | Movement of entityID:uint32
//    | PlantGrowth of entityID:uint32
    | Terrain of TerrainData //entityID:uint32 * TerrainData //{| EntityID:uint32; Terrain:TerrainType |}
    member me.Copy newEID = 
        match me with
        | Form d -> Form (newEID,d)
        | Terrain (_,d) -> Terrain (newEID,d)
    member me.ComponentID =
        match me with
        | Form _ -> 1
        | Terrain _ -> 2
    member me.EntityID =
        match me with
        | Form (e,_) -> e
        | Terrain (e,_) -> e


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


//[<AbstractClass>]
//type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
//    member this.ComponentType = componentType
//    member this.EntityID = eid

//    abstract member Copy : uint32 -> AbstractComponent

//    member this.Abstract = this :> AbstractComponent

