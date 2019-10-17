module EatingComponent
open AbstractComponent
open FoodComponent
open System


type EatingComponent(eid:uint32, foods:FoodTypes[], quantity:int, quantityMax:int, quantityPerAction:int, calories:int) = 
    inherit AbstractComponent(eid,Comp_Eating)

    member _.Calories = calories
    member _.Foods = foods
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax
    member _.QuantityPerAction = quantityPerAction

    static member Type = Comp_Eating
   