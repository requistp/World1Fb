module FoodComponent
open CommonGenericFunctions
open ComponentEnums

[<Struct>]
type FoodComponent(id:ComponentID, eid:EntityID, foodType:FoodTypes, quantity:int, quantityMax:int) =
    member _.ID = id
    member _.EntityID = eid
    member _.FoodType = foodType
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax

  
//let UpdateFood (food:FoodComponent) (foodTypeUpdate:FoodTypes option) (quantityUpdate:int option) (quantityMaxUpdate:int option) =
//       {
//           food with
//               FoodType = if foodTypeUpdate.IsSome then foodTypeUpdate.Value else food.FoodType
//               Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else food.Quantity
//               QuantityMax = if quantityMaxUpdate.IsSome then quantityMaxUpdate.Value else food.QuantityMax
//       }