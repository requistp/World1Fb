module FoodComponent
open AbstractComponent


type FoodClassifications =
    | Meat
    | Plant


type FoodTypes =
    | Food_Carrot
    | Food_Grass
    | Food_Meat_Rabbit
    member this.Calories = // This is calories per Quantity
        match this with 
        | Food_Carrot -> 2
        | Food_Grass -> 1
        | Food_Meat_Rabbit -> 3
    member this.Classification =
        match this with
        | Food_Carrot | Food_Grass -> Plant
        | Food_Meat_Rabbit -> Meat
    member this.Symbol =
        match this.Classification with
        | Plant -> Some '!'
        | Meat -> None


type FoodComponent(eid:uint32, foodType:FoodTypes, quantity:int) = 
    inherit AbstractComponent(eid,Comp_Food)

    member _.FoodType = foodType
    member _.Quantity = quantity

    static member Type = Comp_Food

    