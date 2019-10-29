module FoodSystem
open AbstractSystem
open CalendarTimings
open EntityManager
open FoodComponent
open EventTypes
open GameManager
open PlantGrowthComponent
open SystemManager
open System

type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member private this.onAllEaten (ge:EventData_Generic) =
        match (ge.EntityID |> enm.GetComponent<FoodComponent>).FoodType.KillOnAllEaten with
        | false -> Ok None
        | true -> evm.QueueEvent (EventData_Generic(Kill_AllEaten,ge.EntityID))
                  Ok None

    member private this.onEaten (ge:EventData_Generic) =
        let e = ge :?> EventData_Eaten
        
        match enm.TryGetComponent<FoodComponent> e.EateeID with  // Can't check the game current frame here b/c two entities could have entered their eat action, and the first one could have eaten and killed the food
        | None -> Error "Something else ate it first"
        | Some f -> 
            let allEaten = (f.Quantity - e.Quantity) <= 0
            let result = sprintf "All Eaten:%b" allEaten
            if allEaten then evm.QueueEvent (EventData_Generic(Kill_AllEaten,f.EntityID))
            enm.ReplaceComponent (f.Update None (Some (f.Quantity-e.Quantity)) None) (Some result)

    member private this.onRegrowth (ge:EventData_Generic) =
        let tryRegrowFood (f:FoodComponent) = 
            let pgc = enm.GetComponent<PlantGrowthComponent> ge.EntityID
            let missing = f.QuantityMax - f.Quantity
            match (missing, pgc.RegrowRate) with
            | (0,_) -> Ok None
            | (_,0.0) -> Ok None
            | (_,_) -> 
                let quantity = Math.Clamp((int (Math.Round(pgc.RegrowRate * (float f.QuantityMax),0))), 1, missing)
                let result = sprintf "EntityID:%i. Regrown quantity:%i" ge.EntityID quantity
                enm.ReplaceComponent (f.Update None (Some (f.Quantity+quantity)) None) (Some result)
            
        match (ge.EntityID |> enm.TryGetComponent<FoodComponent>) with
        | None -> Ok None
        | Some food -> tryRegrowFood food
        
    override this.Initialize = 
        evm.RegisterListener "FoodSystem" Eaten this.onEaten
        evm.RegisterListener "FoodSystem" Food_AllEaten this.onAllEaten
        evm.RegisterListener "FoodSystem" PlantRegrowth this.onRegrowth
        base.SetToInitialized

    override this.Update = 
        ()


