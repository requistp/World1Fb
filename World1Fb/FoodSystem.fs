module FoodSystem
open AbstractSystem
open CalendarTimings
open EntityDictionary
open FoodComponent
open GameEvents
open GameManager
open PlantGrowthComponent
open SystemManager
open System

type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    
    member private this.onAllEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Food_AllEaten
        match (e.EntityID |> game.EntityManager.GetComponent<FoodComponent>).FoodType.KillOnAllEaten with
        | false -> Ok None
        | true -> game.EventManager.QueueEvent (Event_Kill_AllEaten(e.EntityID))
                  Ok None

    member private this.onEaten (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eaten
        
        match next.TryGetComponent<FoodComponent> e.EateeID with  // Can't check the game current frame here b/c two entities could have entered their eat action, and the first one could have eaten and killed the food
        | None -> Error "Something else ate it first"
        | Some food -> 
            let allEaten = (food.Quantity - e.Quantity) <= 0
            let result = sprintf "All Eaten:%b" allEaten
            if allEaten then game.EventManager.QueueEvent (Event_Food_AllEaten(food.EntityID))
            next.ReplaceComponent (food.Update(food.Quantity-e.Quantity)) (Some result)

    member private this.onRegrowth (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = ge :?> Event_PlantRegrowth

        let tryRegrowFood (f:FoodComponent) = 
            let pgc = game.EntityManager.GetComponent<PlantGrowthComponent> e.EntityID
            let missing = f.QuantityMax - f.Quantity
            match (missing, pgc.RegrowRate) with
            | (0,_) -> Ok None
            | (_,0.0) -> Ok None
            | (_,_) -> 
                let quantity = Math.Clamp((int (Math.Round(pgc.RegrowRate * (float f.QuantityMax),0))), 1, missing)
                let result = sprintf "EntityID:%i. Quantity:%i" e.EntityID quantity
                next.ReplaceComponent (f.Update(f.Quantity + quantity)) (Some result)
            
        match (e.EntityID |> next.TryGetComponent<FoodComponent>) with
        | None -> Ok None
        | Some foodc -> tryRegrowFood foodc
        
    override this.Initialize = 
        game.EventManager.RegisterListener Eaten this.onEaten
        game.EventManager.RegisterListener Food_AllEaten this.onAllEaten
        game.EventManager.RegisterListener PlantRegrowth this.onRegrowth
        base.SetToInitialized


