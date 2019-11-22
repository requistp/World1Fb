module EatingSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open CalendarTimings
open EatingComponent
open EntityExtensions
open EventManager
open EventTypes
open FoodComponent
open System
open SystemManager
open EntityManager

let FoodsAtLocation (enm:EntityManager) (eat:EatingComponent) =
    eat.EntityID
    |> EntityExt.GetLocation enm None
    |> EntityExt.GetEntitiesAtLocationWithComponent enm None FoodComponentID (Some eat.EntityID)
    |> Array.filter (fun (Food f) -> eat.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining
    |> Array.Parallel.map ToFood

let EatActionEnabled (enm:EntityManager) (entityID:EntityID) =
    let (Eating eat) = enm.GetComponent None EatingComponentID entityID
    (eat.QuantityRemaining > 0) && ((FoodsAtLocation enm eat).Length > 0)

type EatingSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member private me.onEat round (ge:GameEventTypes) =
        let (Eating eat) = ge.EntityID|>enm.GetComponent None EatingComponentID

        let selectFood =
            let foods =
                FoodsAtLocation enm eat
                |> Array.sortByDescending (fun f -> f.FoodType.Calories) // Highest caloric food first
            match foods with 
            | [||] -> None
            | fs -> Some fs.[0]
            
        let eatFood (f:FoodComponent) =
            let quantity = Math.Clamp(eat.QuantityPerAction, 0, Math.Min(f.Quantity,eat.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * f.FoodType.Calories
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> 
                enm.UpdateComponent round (Eating (eat.Update (Some (eat.Quantity+quantity)) (Some (eat.Calories+calories))))
                evm.RaiseEvent (Eaten { EaterID = eat.EntityID; EateeID = f.EntityID; Quantity = quantity })
                Ok (Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID.ToUint32) quantity (eat.Quantity+quantity) calories (eat.Calories+calories)))
        
        match selectFood with
        | None -> Error "No food at location"
        | Some foodEaten -> eatFood foodEaten

    member private me.onComponentAdded round (ge:GameEventTypes) =
        let e = ge.ToComponentAddedEating
        evm.AddToSchedule (ScheduleEvent ({ Schedule = RepeatIndefinitely; Frequency = MetabolismFrequency }, Metabolize { EntityID = e.EntityID }))
        Ok (Some (sprintf "Queued Metabolize to schedule. EntityID:%i" e.EntityID.ToUint32))
        
    member private me.onMetabolize round (ge:GameEventTypes) =
        let (Eating eat) = enm.GetComponent None EatingComponentID ge.EntityID
        let newC = eat.Calories - eat.CaloriesPerMetabolize
        let newQ = eat.Quantity - eat.QuantityPerMetabolize
        let starving = newC < 0
        let result = sprintf "Quantity:-%i=%i. Calories:-%i=%i. Starving:%b" eat.QuantityPerMetabolize newQ eat.CaloriesPerMetabolize newC starving
        if starving then evm.RaiseEvent (Starving { EntityID = eat.EntityID })
        enm.UpdateComponent round (Eating (eat.Update (Some newQ) (Some newC))) 
        Ok (Some result)

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionEat_ID             (me.TrackTask me.onEat)
        evm.RegisterListener me.Description Event_ComponentAdded_Eating_ID (me.TrackTask me.onComponentAdded)
        evm.RegisterListener me.Description Event_Metabolize_ID            (me.TrackTask me.onMetabolize)
        base.SetToInitialized

    override me.Update round = 
        ()


