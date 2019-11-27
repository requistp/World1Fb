module EatingComponent
open CalendarTimings
open CommonGenericFunctions
open ComponentEnums
open System

[<Struct>]
type EatingComponent (id:ComponentID, eid:EntityID, calories:int, caloriesPerDay:int, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int) =
    member _.ID = id
    member _.EntityID = eid
    member _.Calories = calories
    member _.CaloriesPerDay = caloriesPerDay
    member _.Foods = foods
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax
    member _.QuantityPerAction = quantityPerAction

    member me.CaloriesPerMetabolize = Math.Clamp(convertAmountByFrequency me.CaloriesPerDay Day MetabolismFrequency,1,me.CaloriesPerDay)
    member me.QuantityPerMetabolize = Math.Clamp(convertAmountByFrequency me.QuantityMax Day MetabolismFrequency,1,me.QuantityMax)
    member me.QuantityRemaining = me.QuantityMax - me.Quantity
    
let CanEat (eat:EatingComponent) (fd:FoodTypes) = eat.Foods |> Array.contains fd    

//let UpdateEating (eat:EatingComponent) (quantityUpdate:int option) (caloriesUpdate:int option) = 
//       {
//           eat with
//               Quantity = if quantityUpdate.IsSome then quantityUpdate.Value else eat.Quantity
//               Calories = if caloriesUpdate.IsSome then caloriesUpdate.Value else eat.Calories
//       }

