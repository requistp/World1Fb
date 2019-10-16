module EatingSystem
open AbstractComponent
open EatingComponent
open FoodComponent
open FormComponent
open GameEvents
open GameManager
open SystemManager
open System


//type EatingSystem(game:Game, isActive:bool) =
//    inherit AbstractSystem(isActive) 

//    member private this.onEat (ge:AbstractGameEvent) =
//        let e = ge :?> Event_Eat

//        let eater = (game.EntityManager.GetComponent Eating e.EaterID) :?> EatingComponent
//        let food = (game.EntityManager.GetComponent Food e.EateeID) :?> FoodComponent
//        let quantity = Math.Clamp(eater.QuantityPerAction,0,food.Quantity)
//        let calories = quantity * food.FoodType.Calories

//        match eater.Calories_Current = eater.Calories_Max with
//        | true -> ()
//        | false -> this.ChangeLog.AddComponentChange(EatingComponent_Change(e.EaterID, calories))
//                   game.EventManager.QueueEvent(Event_Eaten(food.EntityID, eater.EntityID, quantity))

//    member private this.onKeyPressedEat (ge:AbstractGameEvent) =
//        let e = ge :?> Event_KeyPressed_Eat
        
//        let eaterForm = (game.EntityManager.GetComponent Form e.EntityID) :?> FormComponent
//        let eaterEating = (game.EntityManager.GetComponent Eating e.EntityID) :?> EatingComponent

//        let foodsAtLocation = 
//            game.EntityManager.EntitiesAtLocation eaterForm.Location // Food here
//            |> Array.filter (fun eid -> eid <> eaterForm.EntityID) // Not me
//            |> game.EntityManager.TryGetComponentForEntities Food
//            |> Array.Parallel.map (fun ac -> ac:?> FoodComponent)
//            |> Array.filter (fun f -> eaterEating.Foods |> Array.exists (fun ft -> ft = f.FoodType)) //Types I can eat
//            |> Array.filter (fun f -> f.Quantity > 0) // Food remaining

//        match foodsAtLocation with
//        | [||] -> ()
//        | fs -> game.EventManager.QueueEvent(Event_Eat(e.EntityID,fs.[0].EntityID))

//    override this.Initialize = 
//        game.EventManager.RegisterListener Eat this.onEat
//        game.EventManager.RegisterListener KeyPressed_Eat this.onKeyPressedEat
//        base.SetToInitialized

    //override this.Update = 

