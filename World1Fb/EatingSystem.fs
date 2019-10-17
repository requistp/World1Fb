module EatingSystem
open AbstractComponent
open AbstractSystem
open EatingComponent
open EntityDictionary
open FoodComponent
open FormComponent
open GameEvents
open GameManager
open SystemManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Eating,isActive) 

    member private this.onEat (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Action_Eat
        
        let eater = game.EntityManager.GetComponent<EatingComponent> e.EaterID
        let food = game.EntityManager.GetComponent<FoodComponent> e.EateeID
        let quantity = Math.Clamp(eater.QuantityPerAction, 0, Math.Min(food.Quantity,eater.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
        let calories = quantity * food.FoodType.Calories
        
        match quantity with
        | 0 -> Error "EatingSystem.onEat: No food remaining or stomach is full"
        | _ -> game.EventManager.QueueEvent(Event_Eaten(eater.EntityID, food.EntityID, quantity))
               next.ReplaceComponent (EatingComponent(eater.EntityID, eater.Foods, eater.Quantity+quantity, eater.QuantityMax, eater.QuantityPerAction, eater.Calories+calories))
                       
    override this.Initialize = 
        game.EventManager.RegisterListener Action_Eat this.onEat
        base.SetToInitialized

