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
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    //member private this.onCreateEntity (ge:EventData_Generic) =
    //    let e = ge :?> EventData_CreateEntity 
    //    match e.Components |> Array.filter (fun ct -> ct.ComponentType = Component_Eating) with
    //    | [||] -> Ok (Some "No EatingComponent")
    //    | _ -> 
    //        evm.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(Metabolize,e.EntityID),uint32 MetabolismFrequency)))
    //        Ok (Some "Queued Metabolize to schedule")
        
    //member private this.onMetabolize (ge:EventData_Generic) =
    //    let eatc = enm.GetComponent<EatingComponent> ge.EntityID
    //    let starving = (eatc.Calories-eatc.CaloriesPerMetabolize < 0)
    //    let result = sprintf "Quantity:-%i. Calories:-%i. Starving:%b" eatc.QuantityPerMetabolize eatc.CaloriesPerMetabolize starving
    //    if starving then evm.QueueEvent (EventData_Generic(Starving,ge.EntityID))
    //    enm.ReplaceComponent (eatc.Update (Some (eatc.Quantity-eatc.QuantityPerMetabolize)) (Some (eatc.Calories-eatc.CaloriesPerMetabolize))) (Some result)
        
    //member private this.onEat (ge:EventData_Generic) =
    //    let eco = enm.TryGetComponent<EatingComponent> ge.EntityID
    //    let fco = enm.TryGetComponent<FormComponent> ge.EntityID
    //    let ec = eco.Value
    //    let fc = fco.Value

    //    let foodsAtLocation = 
    //        enm.GetEntitiesAtLocation fc.Location // Food here
    //        |> Array.filter (fun eid -> eid <> ge.EntityID) // Not me
    //        |> enm.TryGetComponentForEntities<FoodComponent>
    //        |> Array.filter (fun f -> ec.Foods|>Array.contains f.FoodType) //Types I can eat
    //        |> Array.filter (fun f -> f.Quantity > 0) // Food remaining
    //        |> Array.sortByDescending (fun x -> x.FoodType.Calories) // Highest caloric food first

    //    let eatIt (f:FoodComponent) =
    //        let quantity = Math.Clamp(ec.QuantityPerAction, 0, Math.Min(f.Quantity,ec.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
    //        let calories = quantity * f.FoodType.Calories
    //        let changes = Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID) quantity (ec.Quantity+quantity) calories (ec.Calories+calories))
    //        match quantity with
    //        | 0 -> Error "Stomach is full"
    //        | _ -> 
    //            evm.QueueEvent (EventData_Eaten(ec.EntityID, f.EntityID, quantity))
    //            enm.ReplaceComponent (ec.Update (Some (ec.Quantity+quantity)) (Some (ec.Calories+calories))) changes
                                
    //    match eco.IsSome && fco.IsSome with
    //    | false -> Error "Missing component"
    //    | true -> 
    //        match foodsAtLocation with
    //        | [||] -> Error "No food at location"
    //        | fs -> eatIt fs.[0]

    override this.Initialize = 
        //evm.RegisterListener "EatingSystem" Action_Eat this.onEat
        ////evm.RegisterListener "EatingSystem" CreateEntity this.onCreateEntity
        //evm.RegisterListener "EatingSystem" Metabolize this.onMetabolize
        base.SetToInitialized

    override this.Update = 
        ()


