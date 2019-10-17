module FoodSystem
open AbstractSystem
open EntityDictionary
open FoodComponent
open GameEvents
open GameManager
open SystemManager
open System

type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Food, isActive) 
    
    member private this.onEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eaten

        let food = game.EntityManager.GetComponent<FoodComponent> e.EateeID

        next.ReplaceComponent (FoodComponent(e.EateeID, food.FoodType, Math.Clamp(food.Quantity-e.Quantity,0,food.QuantityMax), food.QuantityMax))


    override this.Initialize = 
        game.EventManager.RegisterListener Eaten this.onEaten
        base.SetToInitialized


