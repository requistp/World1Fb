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

