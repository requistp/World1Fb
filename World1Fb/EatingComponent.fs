module EatingComponent
open AbstractComponent
open CalendarTimings
open CommonGenericFunctions
open FoodComponent
open System


type EatingComponent(eid:uint32, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int, calories:int, caloriesPerDay:int, timer:ComponentExecutionTimer) = 
    inherit AbstractComponent(eid,Component_Eating)

    static member Type = Component_Eating
    static member Frequency = roundsPerHour * 2

    member _.Calories = calories
    member _.CaloriesPerDay = caloriesPerDay
    member _.Foods = foods
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax
    member _.QuantityPerAction = quantityPerAction
    member _.Timer = timer

    member this.QuantityRemaining = quantityMax - quantity

    member this.Update newQuantity newCalories = 
        EatingComponent(eid, foods, Math.Clamp(newQuantity,0,quantityMax), quantityMax, quantityPerAction, newCalories, caloriesPerDay, timer)

    new (eid:uint32, foods:FoodTypes[], quantityMax:int, quantityPerAction:int, caloriesPerDay:int) =
        EatingComponent(eid, foods, quantityMax/2, quantityMax, quantityPerAction, caloriesPerDay/2, caloriesPerDay, ComponentExecutionTimer(EatingComponent.Frequency))


