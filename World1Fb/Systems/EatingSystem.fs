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
    
    member private me.onEat (ge:GameEventTypes) =
        let e = ge.ToActionEat
        let eco = enm.TryGetComponent EatingComponent.ID e.EntityID
        let fco = enm.TryGetComponent FormComponent.ID e.EntityID
        let ed = eco.Value.ToEating
        let fc = fco.Value.ToForm

        let foodsAtLocation l = 
            enm.GetEntitiesAtLocation l // Food here
            |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
            |> enm.TryGetComponentForEntities FoodComponent.ID
            |> Array.map (fun c -> c.ToFood)
            |> Array.filter (fun f -> ed.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining
            |> Array.sortByDescending (fun f -> f.FoodType.Calories) // Highest caloric food first
            
        let eatIt (f:FoodComponent) =
            let quantity = Math.Clamp(ed.QuantityPerAction, 0, Math.Min(f.Quantity,ed.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            let changes = Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID) quantity (ed.Quantity+quantity) calories (ed.Calories+calories))
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> 
                evm.QueueEvent (Eaten { EaterID=ed.EntityID; EateeID=f.EntityID; Quantity=quantity })
                enm.ReplaceComponent (Eating (ed.Update (Some (ed.Quantity+quantity)) (Some (ed.Calories+calories)))) changes
                                
        match eco.IsSome && fco.IsSome with
        | false -> Error "Missing component"
        | true -> 
            match foodsAtLocation fc.Location with
            | [||] -> Error "No food at location"
            | fs -> eatIt fs.[0]

    member private me.onComponentAdded (ge:GameEventTypes) =
        let e = ge.ToComponentAddedEating
        evm.ScheduleEvent (ScheduleEvent ({ Frequency=uint32 MetabolismFrequency }, Metabolize { EntityID=e.EntityID }))
        Ok (Some (sprintf "Queued Metabolize to schedule. EntityID:%i" e.EntityID))
        
    member private me.onMetabolize (ge:GameEventTypes) =
        printfn "%A\n\n\n" ge
        let e = ge.ToMetabolize
        let ed = (enm.GetComponent EatingComponent.ID e.EntityID).ToEating
        let newC = ed.Calories - ed.CaloriesPerMetabolize
        let newQ = ed.Quantity - ed.QuantityPerMetabolize
        let starving = newC < 0
        let result = sprintf "Quantity:-%i=%i. Calories:-%i=%i. Starving:%b" ed.QuantityPerMetabolize newQ ed.CaloriesPerMetabolize newC starving
        if starving then evm.QueueEvent (Starving { EntityID=e.EntityID })
        enm.ReplaceComponent (Eating (ed.Update (Some newQ) (Some newC))) (Some result)
        
    override me.Initialize = 
        evm.RegisterListener "EatingSystem" Event_ActionEat.ID             me.onEat
        evm.RegisterListener "EatingSystem" Event_ComponentAdded_Eating.ID me.onComponentAdded
        evm.RegisterListener "EatingSystem" Event_Metabolize.ID            me.onMetabolize
        base.SetToInitialized

    override me.Update = 
        ()


