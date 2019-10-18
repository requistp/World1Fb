module EatingSystem
open AbstractSystem
open CalendarTimings
open EatingComponent
open EntityDictionary
open FoodComponent
open GameEvents
open GameManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onMetabolize (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Metabolize

        let eatc = game.EntityManager.GetComponent<EatingComponent> e.EntityID
        let quantity = convertAmountByFrequency eatc.QuantityMax Day EatingComponent.CaloricCheckFrequency
        let calories = convertAmountByFrequency eatc.CaloriesPerDay Day EatingComponent.CaloricCheckFrequency
        let result = sprintf "Quantity:%i. Calories:%i" quantity calories

        next.ReplaceComponent (eatc.Update (eatc.Quantity-quantity) (eatc.Calories-calories)) (Some result)

    member private this.onEat (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Action_Eat
        
        let eatc = game.EntityManager.GetComponent<EatingComponent> e.EaterID
        let foodc = game.EntityManager.GetComponent<FoodComponent> e.EateeID
        let quantity = Math.Clamp(eatc.QuantityPerAction, 0, Math.Min(foodc.Quantity,eatc.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
        let calories = quantity * foodc.FoodType.Calories
        let result = sprintf "Quantity:%i. Calories:%i" quantity calories
        
        match quantity with
        | 0 -> Error "EatingSystem.onEat: No food remaining or stomach is full"
        | _ -> game.EventManager.QueueEvent(Event_Eaten(eatc.EntityID, foodc.EntityID, quantity))
               next.ReplaceComponent (eatc.Update (eatc.Quantity+quantity) (eatc.Calories+calories)) (Some result)

    member private this.UpdateMetabolism r =
        game.EntityManager.EntitiesWithComponent EatingComponent.Type
        |> game.EntityManager.GetComponent<EatingComponent>
        |> Array.filter (fun ec -> ec.ExecuteTiming r)
        |> Array.iter (fun ec -> game.EventManager.QueueEvent (Event_Metabolize(ec.EntityID)))

    override this.Initialize = 
        game.EventManager.RegisterListener Action_Eat this.onEat
        game.EventManager.RegisterListener Metabolize this.onMetabolize
        base.SetToInitialized

    override this.Update r = 
        this.UpdateMetabolism r