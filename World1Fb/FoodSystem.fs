module FoodSystem
open AbstractSystem
open EntityDictionary
open FoodComponent
open GameEvents
open GameManager
open SystemManager
open System

type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    member private this.onAllEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Food_AllEaten

        match (e.EntityID |> game.EntityManager.GetComponent<FoodComponent>).FoodType.KillOnAllEaten with
        | false -> Ok (Some "No effect (not killed)")
        | true -> game.EventManager.QueueEvent (Event_Kill_AllEaten(e.EntityID))
                  Ok (Some "Killed")

    member private this.onEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eaten

        let food = game.EntityManager.GetComponent<FoodComponent> e.EateeID
        let allEaten = (food.Quantity - e.Quantity) < 0
        let result = sprintf "All Eaten:%b" allEaten

        if allEaten then game.EventManager.QueueEvent (Event_Food_AllEaten(food.EntityID))

        next.ReplaceComponent (food.Update(food.Quantity - e.Quantity)) (if result="" then Some result else None)


    override this.Initialize = 
        game.EventManager.RegisterListener Eaten this.onEaten
        game.EventManager.RegisterListener Food_AllEaten this.onAllEaten
        base.SetToInitialized


