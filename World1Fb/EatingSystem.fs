module EatingSystem
open AbstractComponent
open AbstractSystem
open EatingComponent
open EntityDictionary
open FoodComponent
open FormComponent
open GameEvents
open GameManager
open SystemManager
open System


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Eating,isActive) 

    member private this.onEat (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Action_Eat
        //e.
        Ok None
    //    let eater = (game.EntityManager.GetComponent Eating e.EaterID) :?> EatingComponent
    //    let food = (game.EntityManager.GetComponent Food e.EateeID) :?> FoodComponent
    //    let quantity = Math.Clamp(eater.QuantityPerAction,0,food.Quantity)
    //    let calories = quantity * food.FoodType.Calories

    //    match eater.Calories_Current = eater.Calories_Max with
    //    | true -> ()
    //    | false -> this.ChangeLog.AddComponentChange(EatingComponent_Change(e.EaterID, calories))
    //               game.EventManager.QueueEvent(Event_Eaten(food.EntityID, eater.EntityID, quantity))

    override this.Initialize = 
        game.EventManager.RegisterListener Action_Eat this.onEat
        base.SetToInitialized


    //member this.OnKeyPressedEat (eid:uint32) =
    //    let eaterForm = (game.EntityManager.GetComponent Comp_Form eid) :?> FormComponent
    //    let eaterEating = (game.EntityManager.GetComponent Comp_Eating eid) :?> EatingComponent

    //    let foodsAtLocation = 
    //        game.EntityManager.EntitiesAtLocation eaterForm.Location // Food here
    //        |> Array.filter (fun eid -> eid <> eaterForm.EntityID) // Not me
    //        |> game.EntityManager.TryGetComponentForEntities Comp_Food
    //        |> Array.Parallel.map (fun ac -> ac:?> FoodComponent)
    //        |> Array.filter (fun f -> eaterEating.Foods |> Array.exists (fun ft -> ft = f.FoodType)) //Types I can eat
    //        |> Array.filter (fun f -> f.Quantity > 0) // Food remaining

    //    match foodsAtLocation with
    //    | [||] -> ()
    //    | fs -> printfn "raising eating event, have eaten item"; game.EventManager.QueueEvent(Event_Action_Eat(eid,fs.[0].EntityID))
