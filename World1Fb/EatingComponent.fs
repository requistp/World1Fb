module EatingComponent
open AbstractComponent
open CalendarTimings
open CommonGenericFunctions
open FoodComponent
open System


type EatingComponent(eid:uint32, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int, calories:int, caloriesPerDay:int, caloricCheckOffset:int) = 
    inherit AbstractComponent(eid,Comp_Eating)
    
    static member Type = Comp_Eating
    static member CaloricCheckFrequency = roundsPerHour * 2

    member _.CaloricCheckOffset = caloricCheckOffset
    member _.Calories = calories
    member _.CaloriesPerDay = caloriesPerDay
    member _.Foods = foods
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax
    member _.QuantityPerAction = quantityPerAction

    member this.ExecuteTiming round = ExecuteTiming EatingComponent.CaloricCheckFrequency (int caloricCheckOffset) round
    member this.QuantityRemaining = quantityMax - quantity

    member this.Update newQuantity newCalories = 
        EatingComponent(eid, foods, Math.Clamp(newQuantity,0,quantityMax), quantityMax, quantityPerAction, newCalories, caloriesPerDay, caloricCheckOffset)

    new (eid:uint32, foods:FoodTypes[], quantityMax:int, quantityPerAction:int, caloriesPerDay:int) =
        EatingComponent(eid, foods, quantityMax/2, quantityMax, quantityPerAction, caloriesPerDay/2, caloriesPerDay, TimingOffset(EatingComponent.CaloricCheckFrequency))