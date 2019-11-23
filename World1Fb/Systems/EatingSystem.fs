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
    |> EntityExt.GetEntitiesAtLocationWithComponent enm None FoodComponent (Some eat.EntityID)
    |> Array.filter (fun (Food f) -> eat.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining
    |> Array.Parallel.map ToFood

let EatActionEnabled (enm:EntityManager) (entityID:EntityID) =
    let (Eating eat) = enm.GetComponent None EatingComponent entityID
    (eat.QuantityRemaining > 0) && ((FoodsAtLocation enm eat).Length > 0)

type EatingSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member private me.onEat round (Action_Eat eat:GameEventData) =
        let selectFood =
            let foods =
                FoodsAtLocation enm eat
                |> Array.sortByDescending (fun f -> f.FoodType.Calories) // Highest caloric food first
            match foods with 
            | [||] -> None
            | fs -> Some fs.[0]
            
        let eatFood (food:FoodComponent) =
            let quantity = Math.Clamp(eat.QuantityPerAction, 0, Math.Min(food.Quantity,eat.QuantityRemaining)) // Clamp by how much food is left and how much stomach space is left
            let calories = quantity * food.FoodType.Calories
            match quantity with
            | 0 -> Error "Stomach is full"
            | _ -> 
                enm.UpdateComponent round (Eating (eat.Update (Some (eat.Quantity+quantity)) (Some (eat.Calories+calories))))
                evm.RaiseEvent (Eaten (eat,food))
                Ok (Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (food.EntityID.ToUint32) quantity (eat.Quantity+quantity) calories (eat.Calories+calories)))
        
        match selectFood with
        | None -> Error "No food at location"
        | Some foodEaten -> eatFood foodEaten

    member private me.onComponentAdded round (ComponentAdded_Eating eat:GameEventData) =
        evm.AddToSchedule (ScheduleEvent (MetabolismFrequency, RepeatIndefinitely, Metabolize eat))
        Ok (Some (sprintf "Queued Metabolize to schedule. EntityID:%i" eat.EntityID.ToUint32))
        
    member private me.onMetabolize round (Metabolize eat:GameEventData) =
        let newC = eat.Calories - eat.CaloriesPerMetabolize
        let newQ = eat.Quantity - eat.QuantityPerMetabolize
        let starving = newC < 0
        let result = sprintf "Quantity:-%i=%i. Calories:-%i=%i. Starving:%b" eat.QuantityPerMetabolize newQ eat.CaloriesPerMetabolize newC starving
        if starving then evm.RaiseEvent (Starving eat) 
        enm.UpdateComponent round (Eating (eat.Update (Some newQ) (Some newC))) 
        Ok (Some result)

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionEat             (me.TrackTask me.onEat)
        evm.RegisterListener me.Description Event_ComponentAdded_Eating (me.TrackTask me.onComponentAdded)
        evm.RegisterListener me.Description Event_Metabolize            (me.TrackTask me.onMetabolize)
        base.SetToInitialized

    override me.Update round = 
        ()


