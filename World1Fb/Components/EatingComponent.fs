module EatingComponent
open AbstractComponent
open CalendarTimings
open FoodComponent
open System


type EatingComponent(eid:uint32, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int, calories:int, caloriesPerDay:int) = 
    inherit AbstractComponent(eid,Component_Eating)
    static member Type = Component_Eating

    member _.Calories = calories
    member _.CaloriesPerDay = caloriesPerDay
    member _.Foods = foods
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax
    member _.QuantityPerAction = quantityPerAction

    member this.CaloriesPerMetabolize = Math.Clamp(convertAmountByFrequency caloriesPerDay Day MetabolismFrequency,1,caloriesPerDay)
    member this.QuantityPerMetabolize = Math.Clamp(convertAmountByFrequency quantityMax Day MetabolismFrequency,1,quantityMax)
    member this.QuantityRemaining = quantityMax - quantity

    member this.Update newQuantity newCalories = 
        EatingComponent(eid, foods, Math.Clamp(newQuantity,0,quantityMax), quantityMax, quantityPerAction, newCalories, caloriesPerDay)

    new (eid:uint32, foods:FoodTypes[], quantityMax:int, quantityPerAction:int, caloriesPerDay:int) =
        EatingComponent(eid, foods, quantityMax/2, quantityMax, quantityPerAction, caloriesPerDay/2, caloriesPerDay)


    override this.Copy neweid = 
        EatingComponent(neweid, foods, quantity, quantityMax, quantityPerAction, calories, caloriesPerDay).Abstract

