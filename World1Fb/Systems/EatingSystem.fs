module EatingSystem
open AbstractComponent
open AbstractSystem
open CalendarTimings
open EatingComponent
open EntityManager
open FoodComponent
open FormComponent
open EventTypes
open GameManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onCreateEntity (enm:EntityManager) (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity 
        match e.Components |> Array.filter (fun ct -> ct.ComponentType = Component_Eating) with
        | [||] -> Ok (Some "No EatingComponent")
        | ct -> game.EventManager.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(Metabolize,e.EntityID),uint32 MetabolismFrequency)))
                Ok (Some "Queued Metabolize to schedule")

    member private this.onMetabolize (enm:EntityManager) (ge:EventData_Generic) =
        let eatc = enm.GetComponent<EatingComponent> ge.EntityID
        let starving = (eatc.Calories-eatc.CaloriesPerMetabolize < 0)
        let result = sprintf "Quantity:-%i. Calories:-%i. Starving:%b" eatc.QuantityPerMetabolize eatc.CaloriesPerMetabolize starving
        if starving then game.EventManager.QueueEvent (EventData_Generic(Starving,ge.EntityID))
        enm.ReplaceComponent (eatc.Update (eatc.Quantity-eatc.QuantityPerMetabolize) (eatc.Calories-eatc.CaloriesPerMetabolize)) (Some result)
        
    member private this.onEat (enm:EntityManager) (ge:EventData_Generic) =
        let eatc = enm.GetComponent<EatingComponent> ge.EntityID
        let formc = enm.GetComponent<FormComponent> ge.EntityID

        let foodsAtLocation = 
            enm.Locations_Next.Get formc.Location // Food here
            |> Array.filter (fun eid -> eid <> ge.EntityID) // Not me
            |> enm.TryGetComponentForEntities<FoodComponent>
            |> Array.filter (fun f -> eatc.Foods |> Array.exists (fun ft -> ft = f.FoodType)) //Types I can eat
            |> Array.filter (fun f -> f.Quantity > 0) // Food remaining
            |> Array.sortByDescending (fun x -> x.FoodType.Calories) // Highest caloric food first

        let eatIt (f:FoodComponent) =
            let quantity = Math.Clamp(eatc.QuantityPerAction, 0, Math.Min(f.Quantity,eatc.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            let result = sprintf "EateeID: %i. Quantity:%i. Calories:%i" (f.EntityID) quantity calories
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> game.EventManager.QueueEvent(EventData_Eaten(eatc.EntityID, f.EntityID, quantity))
                   enm.ReplaceComponent (eatc.Update (eatc.Quantity+quantity) (eatc.Calories+calories)) (Some result)

        match foodsAtLocation with
        | [||] -> Error "No food at location"
        | fs -> eatIt fs.[0]
        
    override this.Initialize = 
        game.EventManager.RegisterListener Action_Eat this.onEat
        game.EventManager.RegisterListener CreateEntity this.onCreateEntity
        game.EventManager.RegisterListener Metabolize this.onMetabolize
        base.SetToInitialized

    override this.Update = 
        ()


