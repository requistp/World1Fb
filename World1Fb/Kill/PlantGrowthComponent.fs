module PlantGrowthComponent
open Component
//open TerrainComponent

//type PlantGrowthComponent(eid:uint32, growsInTerrain:TerrainType[], regrowRate:float, reproductionRate:float, reproductionRange:int, reproductionRequiredFoodQuantity:float) = 
//    inherit AbstractComponent(eid,Component_PlantGrowth)
    
//    member _.GrowsInTerrain = growsInTerrain
//    member _.RegrowRate = regrowRate
//    member _.ReproductionRate = reproductionRate
//    member _.ReproductionRange = reproductionRange
//    member _.ReproductionRequiredFoodQuantity = reproductionRequiredFoodQuantity
    
//    override this.Copy neweid = 
//        PlantGrowthComponent(neweid, growsInTerrain, regrowRate, reproductionRate, reproductionRange, reproductionRequiredFoodQuantity).Abstract
    

    
    
    //type ComponentTypes =
    //    | Component_Controller 
    //    | Component_Eating     
    //    | Component_Food       
    //    | Component_Form       
    //    | Component_Movement   
    //    | Component_PlantGrowth
    //    | Component_Terrain
    //    with 
    //    static member AsArray = 
    //        [|
    //            Component_Controller 
    //            Component_Eating     
    //            Component_Food       
    //            Component_Form       
    //            Component_Movement   
    //            Component_PlantGrowth
    //            Component_Terrain    
    //        |]
    
    
    //[<AbstractClass>]
    //type AbstractComponent(eid:uint32, componentType:ComponentTypes) =
    //    member this.ComponentType = componentType
    //    member this.EntityID = eid
    
    //    abstract member Copy : uint32 -> AbstractComponent
    
    //    member this.Abstract = this :> AbstractComponent
    
    