module FoodComponent
open CommonGenericFunctions
open ComponentEnums


type FoodComponent = 
    { 
        ID : ComponentID
        EntityID : EntityID
        FoodType : FoodTypes
        Quantity : int
        QuantityMax : int 
    }
  
let UpdateFood (food:FoodComponent) (foodTypeUpdate:FoodTypes option) (quantityUpdate:int option) (quantityMaxUpdate:int option) =
       {
           food with
               FoodType = if foodTypeUpdate.IsSome then foodTypeUpdate.Value else food.FoodType
               Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else food.Quantity
               QuantityMax = if quantityMaxUpdate.IsSome then quantityMaxUpdate.Value else food.QuantityMax
       }