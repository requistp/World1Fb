module EatingComponent
open CalendarTimings
open ComponentEnums
open System


type EatingComponent = 
    { 
        EntityID:uint32
        Calories:int
        CaloriesPerDay:int
        Foods:FoodTypes[]
        Quantity:int
        QuantityMax:int
        QuantityPerAction:int 
    } with 

    member me.CaloriesPerMetabolize = Math.Clamp(convertAmountByFrequency me.CaloriesPerDay Day MetabolismFrequency,1,me.CaloriesPerDay)
    
    member me.CanEat (fd:FoodTypes) = me.Foods |> Array.contains fd    
    
    member me.QuantityPerMetabolize = Math.Clamp(convertAmountByFrequency me.QuantityMax Day MetabolismFrequency,1,me.QuantityMax)
    
    member me.QuantityRemaining = me.QuantityMax - me.Quantity
    
    member me.Update (quantityUpdate:int option) (caloriesUpdate:int option) = 
        {
            me with
                Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else me.Quantity
                Calories = if caloriesUpdate.IsSome then caloriesUpdate.Value else me.Calories
        }


