module FoodComponent
open ComponentEnums


type FoodComponent = 
    { 
        EntityID : uint32
        FoodType : FoodTypes
        Quantity : int
        QuantityMax : int 
    }
    member me.Update (foodTypeUpdate:FoodTypes option) (quantityUpdate:int option) (quantityMaxUpdate:int option) =
        {
            me with
                FoodType = if foodTypeUpdate.IsSome then foodTypeUpdate.Value else me.FoodType
                Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else me.Quantity
                QuantityMax = if quantityMaxUpdate.IsSome then quantityMaxUpdate.Value else me.QuantityMax
        }
    