module EatingSystem
open Component
open ComponentEnums
open CalendarTimings
open EventTypes
open FoodComponent
open GameManager
open System
open SystemManager


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member private me.onEat (ge:GameEventTypes) =
        let e = ge.ToActionEat
        let eco = enm.TryGetComponent EatingComponentID e.EntityID
        let fco = enm.TryGetComponent FormComponentID e.EntityID
        let ed = eco.Value.ToEating
        let fc = fco.Value.ToForm

        let foodsAtLocation = 
            fc.Location
            |> enm.GetEntitiesAtLocation // Food here
            |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
            |> enm.TryGetComponentForEntities FoodComponentID
            |> Array.map (fun c -> c.ToFood)
            |> Array.filter (fun f -> ed.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining
            |> Array.sortByDescending (fun f -> f.FoodType.Calories) // Highest caloric food first
            
        let eatIt (f:FoodComponent) =
            let quantity = Math.Clamp(ed.QuantityPerAction, 0, Math.Min(f.Quantity,ed.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> 
                evm.RaiseEvent (Eaten { EaterID=ed.EntityID; EateeID=f.EntityID; Quantity=quantity })
                enm.ReplaceComponent (Eating (ed.Update (Some (ed.Quantity+quantity)) (Some (ed.Calories+calories)))) 
                Ok (Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID) quantity (ed.Quantity+quantity) calories (ed.Calories+calories)))
                                
        match eco.IsSome && fco.IsSome with
        | false -> Error "Missing component"
        | true -> 
            match foodsAtLocation with
            | [||] -> Error "No food at location"
            | fs -> eatIt fs.[0]

    member private me.onComponentAdded (ge:GameEventTypes) =
        let e = ge.ToComponentAddedEating
        evm.ScheduleEvent (ScheduleEvent ({ Schedule=RepeatIndefinitely; Frequency=uint32 MetabolismFrequency }, Metabolize { EntityID=e.EntityID }))
        Ok (Some (sprintf "Queued Metabolize to schedule. EntityID:%i" e.EntityID))
        
    member private me.onMetabolize (ge:GameEventTypes) =
        let e = ge.ToMetabolize
        let ed = (enm.GetComponent EatingComponentID e.EntityID).ToEating
        let newC = ed.Calories - ed.CaloriesPerMetabolize
        let newQ = ed.Quantity - ed.QuantityPerMetabolize
        let starving = newC < 0
        let result = sprintf "Quantity:-%i=%i. Calories:-%i=%i. Starving:%b" ed.QuantityPerMetabolize newQ ed.CaloriesPerMetabolize newC starving
        if starving then evm.RaiseEvent (Starving { EntityID=e.EntityID })
        enm.ReplaceComponent (Eating (ed.Update (Some newQ) (Some newC))) 
        Ok (Some result)

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ActionEat_ID             me.onEat
        evm.RegisterListener me.ToString Event_ComponentAdded_Eating_ID me.onComponentAdded
        evm.RegisterListener me.ToString Event_Metabolize_ID            me.onMetabolize
        base.SetToInitialized

    override _.ToString = "EatingSystem"
       
    override me.Update round = 
        ()


