module EatingSystem
open Component
open ComponentEnums
open AbstractSystem
open CalendarTimings
open EntityManager
open EventTypes
open GameManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member private this.onCreateEntity (ge:EventData_Generic) =
        let e = ge :?> EventData_CreateEntity 
        match e.Components |> Array.filter (fun ct -> ct.ComponentID = EatingData.ID) with
        | [||] -> Ok (Some "No EatingComponent")
        | _ -> 
            evm.QueueEvent (EventData_ScheduleEvent(e.EntityID, ScheduledEvent(EventData_Generic(Metabolize,e.EntityID),uint32 MetabolismFrequency)))
            Ok (Some "Queued Metabolize to schedule")
        
    member private me.onMetabolize (ge:EventData_Generic) =
        let (Eating ed) = enm.GetComponent EatingData.ID ge.EntityID
        let starving = (ed.Calories-ed.CaloriesPerMetabolize < 0)
        let result = sprintf "Quantity:-%i. Calories:-%i. Starving:%b" ed.QuantityPerMetabolize ed.CaloriesPerMetabolize starving
        if starving then evm.QueueEvent (EventData_Generic(Starving,ge.EntityID))
        enm.ReplaceComponent (Eating (ed.Update (Some (ed.Quantity-ed.QuantityPerMetabolize)) (Some (ed.Calories-ed.CaloriesPerMetabolize)))) (Some result)
        
    member private this.onEat (ge:EventData_Generic) =
        let eco = enm.TryGetComponent EatingData.ID ge.EntityID
        let fco = enm.TryGetComponent FormData.ID ge.EntityID
        let (Eating ed) = eco.Value
        let (Form fc) = fco.Value

        let foodsAtLocation l = 
            enm.GetEntitiesAtLocation l // Food here
            |> Array.filter (fun eid -> eid <> ge.EntityID) // Not me
            |> enm.TryGetComponentForEntities FoodData.ID
            |> Array.map (fun c -> c.ToFood)
            |> Array.filter (fun f -> ed.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining
            |> Array.sortByDescending (fun f -> f.FoodType.Calories) // Highest caloric food first
            
        let eatIt f =
            let quantity = Math.Clamp(ed.QuantityPerAction, 0, Math.Min(f.Quantity,ed.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            let changes = Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID) quantity (ed.Quantity+quantity) calories (ed.Calories+calories))
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> 
                evm.QueueEvent (EventData_Eaten(ed.EntityID, f.EntityID, quantity))
                enm.ReplaceComponent (Eating (ed.Update (Some (ed.Quantity+quantity)) (Some (ed.Calories+calories)))) changes
                                
        match eco.IsSome && fco.IsSome with
        | false -> Error "Missing component"
        | true -> 
            match foodsAtLocation fc.Location with
            | [||] -> Error "No food at location"
            | fs -> eatIt fs.[0]

    override this.Initialize = 
        evm.RegisterListener "EatingSystem" Action_Eat this.onEat
        evm.RegisterListener "EatingSystem" CreateEntity this.onCreateEntity
        evm.RegisterListener "EatingSystem" Metabolize this.onMetabolize
        base.SetToInitialized

    override this.Update = 
        ()


