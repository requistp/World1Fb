module FoodComponent
open Component
open System


//type FoodComponent(eid:uint32, foodType:FoodTypes, quantity:int, quantityMax:int) = 
//    inherit AbstractComponent(eid,Component_Food)

//    member _.FoodType = foodType
//    member _.Quantity = quantity
//    member _.QuantityMax = quantityMax

//    member this.Update (foodTypeUpdate:FoodTypes option) (quantityUpdate:int option) (quantityMaxUpdate:int option) =
//        FoodComponent(eid, 
//            (if foodTypeUpdate.IsSome then foodTypeUpdate.Value else foodType),
//            (if quantityUpdate.IsSome then quantityUpdate.Value else quantity),
//            (if quantityMaxUpdate.IsSome then quantityMaxUpdate.Value else quantityMax)
//            )

//    new (eid:uint32, foodType:FoodTypes, quantityMax:int) =
//        FoodComponent(eid, foodType, quantityMax, quantityMax)

//    override this.Copy neweid = 
//        FoodComponent(neweid, foodType, quantity, quantityMax).Abstract


