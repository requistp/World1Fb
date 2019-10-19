module EatingSystem
open AbstractSystem
open CalendarTimings
open EatingComponent
open EntityDictionary
open FoodComponent
open FormComponent
open GameEvents
open GameManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onMetabolize (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Metabolize

        let eatc = game.EntityManager.GetComponent<EatingComponent> e.EntityID
        let quantity = convertAmountByFrequency eatc.QuantityMax Day EatingComponent.Frequency
        let calories = convertAmountByFrequency eatc.CaloriesPerDay Day EatingComponent.Frequency
        let starving = (eatc.Quantity-quantity < 0)
        let result = sprintf "Quantity:-%i. Calories:-%i. Starving:%b" quantity calories starving

        if starving then game.EventManager.QueueEvent (Event_Starving(e.EntityID))

        next.ReplaceComponent (eatc.Update (eatc.Quantity-quantity) (eatc.Calories-calories)) (Some result)
        
    member private this.onEat (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Action_Eat
        
        let eatc = game.EntityManager.GetComponent<EatingComponent> e.EntityID
        let formc = game.EntityManager.GetComponent<FormComponent> e.EntityID

        let foodsAtLocation = 
            next.EntitiesAtLocation formc.Location // Food here
            |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
            |> next.TryGetComponentForEntities<FoodComponent>
            |> Array.filter (fun f -> eatc.Foods |> Array.exists (fun ft -> ft = f.FoodType)) //Types I can eat
            |> Array.filter (fun f -> f.Quantity > 0) // Food remaining
            |> Array.sortByDescending (fun x -> x.FoodType.Calories) // Highest caloric food first

        let eatIt (f:FoodComponent) =
            let quantity = Math.Clamp(eatc.QuantityPerAction, 0, Math.Min(f.Quantity,eatc.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            let result = sprintf "EateeID: %i. Quantity:%i. Calories:%i" (f.EntityID) quantity calories
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> game.EventManager.QueueEvent(Event_Eaten(eatc.EntityID, f.EntityID, quantity))
                   next.ReplaceComponent (eatc.Update (eatc.Quantity+quantity) (eatc.Calories+calories)) (Some result)

        match foodsAtLocation with
        | [||] -> Error "No food at location"
        | fs -> eatIt fs.[0]

    member private this.UpdateMetabolism r =
        game.EntityManager.EntitiesWithComponent EatingComponent.Type
        |> game.EntityManager.GetComponent<EatingComponent>
        |> Array.filter (fun ec -> ec.Timer.Execute r)
        |> Array.iter (fun ec -> game.EventManager.QueueEvent (Event_Metabolize(ec.EntityID)))
        
    override this.Initialize = 
        game.EventManager.RegisterListener Action_Eat this.onEat
        game.EventManager.RegisterListener Metabolize this.onMetabolize
        base.SetToInitialized

    override this.Update r = 
        this.UpdateMetabolism r