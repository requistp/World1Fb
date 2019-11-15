﻿module EatingSystem
open Component
open ComponentEnums
open CalendarTimings
open EatingComponent
open EntityManager
open EventManager
open EventTypes
open FoodComponent
open System
open SystemManager
open Entities

type EatingSystem(description:string, isActive:bool, enm:Entities, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    static let foodsAtLocation (enm:Entities) (eat:EatingComponent) =
        eat.EntityID
        |> Entities.GetLocation enm
        |> Entities.GetEntitiesAtLocationWithComponent enm FoodComponentID (Some eat.EntityID)
        |> Array.Parallel.map (fun c -> c.ToFood)
        |> Array.filter (fun f -> eat.CanEat f.FoodType && f.Quantity > 0) // Types I can eat & Food remaining

    static member EatActionEnabled (enm:Entities) (entityID:uint32) =
        let eat = (entityID|>enm.GetComponent EatingComponentID).ToEating
        (eat.QuantityRemaining > 0) && ((foodsAtLocation enm eat).Length > 0)
        
    member private me.onEat round (ge:GameEventTypes) =
        let eat = (ge.EntityID |> enm.GetComponent EatingComponentID).ToEating

        let selectFood =
            let foods =
                foodsAtLocation enm eat
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
                evm.RaiseEvent (Eaten { EaterID=eat.EntityID; EateeID=f.EntityID; Quantity=quantity })
                enm.ReplaceComponent (Eating (eat.Update (Some (eat.Quantity+quantity)) (Some (eat.Calories+calories)))) 
                Ok (Some (sprintf "EateeID: %i. Quantity: +%i=%i. Calories: +%i=%i" (f.EntityID) quantity (eat.Quantity+quantity) calories (eat.Calories+calories)))
        
        match selectFood with
        | None -> Error "No food at location"
        | Some foodEaten -> eatFood foodEaten

    member private me.onComponentAdded round (ge:GameEventTypes) =
        let e = ge.ToComponentAddedEating
        evm.AddToSchedule (ScheduleEvent ({ Schedule=RepeatIndefinitely; Frequency=uint32 MetabolismFrequency }, Metabolize { EntityID=e.EntityID }))
        Ok (Some (sprintf "Queued Metabolize to schedule. EntityID:%i" e.EntityID))
        
    member private me.onMetabolize round (ge:GameEventTypes) =
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
        evm.RegisterListener me.Description Event_ActionEat_ID             (me.TrackTask me.onEat)
        evm.RegisterListener me.Description Event_ComponentAdded_Eating_ID (me.TrackTask me.onComponentAdded)
        evm.RegisterListener me.Description Event_Metabolize_ID            (me.TrackTask me.onMetabolize)
        base.SetToInitialized

    override me.Update round = 
        ()


