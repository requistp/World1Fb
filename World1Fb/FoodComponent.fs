﻿module FoodComponent
open AbstractComponent
open System

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
    member this.KillOnAllEaten =
        match this with 
        | Food_Grass | Food_Meat_Rabbit -> false
        | Food_Carrot -> true
    member this.Symbol =
        match this.Classification with
        | Plant -> Some '!'
        | Meat -> None


type FoodComponent(eid:uint32, foodType:FoodTypes, quantity:int, quantityMax:int) = 
    inherit AbstractComponent(eid,Comp_Food)

    static member Type = Comp_Food

    member _.FoodType = foodType
    member _.Quantity = quantity
    member _.QuantityMax = quantityMax

    member this.Update (newQuantity:int) =
        FoodComponent(eid, foodType, Math.Clamp(newQuantity,0,quantityMax), quantityMax)

    new (eid:uint32, foodType:FoodTypes, quantityMax:int) =
        FoodComponent(eid, foodType, quantityMax, quantityMax)


