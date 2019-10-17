module FoodSystem
open AbstractSystem
open EntityDictionary
open FoodComponent
open GameEvents
open GameManager
open SystemManager
open System

type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    member private this.onEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eaten

        let food = game.EntityManager.GetComponent<FoodComponent> e.EateeID
        
        let result = ""

        next.ReplaceComponent (food.Update(food.Quantity - e.Quantity)) (Some result)


    override this.Initialize = 
        game.EventManager.RegisterListener Eaten this.onEaten
        base.SetToInitialized


