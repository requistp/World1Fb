module EatingComponent
open Component
open CalendarTimings
//open FoodComponent
open System


//type EatingComponent(eid:uint32, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int, calories:int, caloriesPerDay:int) = 
//    inherit AbstractComponent(eid,Component_Eating)

//    member _.Calories = calories
//    member _.CaloriesPerDay = caloriesPerDay
//    member _.Foods = foods
//    member _.Quantity = quantity
//    member _.QuantityMax = quantityMax
//    member _.QuantityPerAction = quantityPerAction

//    member this.CaloriesPerMetabolize = Math.Clamp(convertAmountByFrequency caloriesPerDay Day MetabolismFrequency,1,caloriesPerDay)
//    member this.QuantityPerMetabolize = Math.Clamp(convertAmountByFrequency quantityMax Day MetabolismFrequency,1,quantityMax)
//    member this.QuantityRemaining = quantityMax - quantity

//    member this.Update (newQuantity:int option) (newCalories:int option) = 
//        let newQ = if newQuantity.IsSome then newQuantity.Value else quantity
//        let newC = if newCalories.IsSome then newCalories.Value else calories
//        EatingComponent(eid, foods, Math.Clamp(newQ,0,quantityMax), quantityMax, quantityPerAction, newC, caloriesPerDay)

//    new (eid:uint32, foods:FoodTypes[], quantityMax:int, quantityPerAction:int, caloriesPerDay:int) =
//        EatingComponent(eid, foods, quantityMax/2, quantityMax, quantityPerAction, caloriesPerDay/2, caloriesPerDay)

//    override this.Copy neweid = 
//        EatingComponent(neweid, foods, quantity, quantityMax, quantityPerAction, calories, caloriesPerDay).Abstract

    