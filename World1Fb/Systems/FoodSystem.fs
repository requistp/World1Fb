module FoodSystem
open AbstractSystem
open CalendarTimings
open Component
open EntityManager
open EventTypes
open GameManager
open SystemManager
open System


type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member private me.onAllEaten (ge:EventData_Generic) =
        match (ge.EntityID|>enm.GetComponent FoodData.ID).ToFood.FoodType.KillOnAllEaten with
        | false -> Ok None
        | true -> 
            evm.QueueEvent (EventData_Generic(Kill_AllEaten,ge.EntityID))
            Ok None

    member private me.onEaten (ge:EventData_Generic) =
        let e = ge :?> EventData_Eaten
        
        match enm.TryGetComponent FoodData.ID e.EateeID with
        | None -> Error "Something else ate it first"
        | Some c -> 
            let (Food f) = c
            let allEaten = (f.Quantity - e.Quantity) <= 0
            let changes = Some (sprintf "All eaten:%b" allEaten)
            if allEaten then evm.QueueEvent (EventData_Generic(Kill_AllEaten,f.EntityID))
            enm.ReplaceComponent (Food (f.Update None (Some (f.Quantity-e.Quantity)) None)) changes
       
    member private me.onRegrowth (ge:EventData_Generic) =
        let tryRegrowFood (f:FoodData) = 
            let (PlantGrowth pg) = enm.GetComponent PlantGrowthData.ID ge.EntityID
            let missing = f.QuantityMax - f.Quantity
            match (missing, pg.RegrowRate) with
            | (0,_) -> Ok None
            | (_,0.0) -> Ok None
            | (_,_) -> 
                let quantity = Math.Clamp((int (Math.Round(pg.RegrowRate * (float f.QuantityMax),0))), 1, missing)
                let result = sprintf "EntityID:%i. Regrown quantity:%i" ge.EntityID quantity
                enm.ReplaceComponent (Food (f.Update None (Some (f.Quantity+quantity)) None)) (Some result)
        match (ge.EntityID |> enm.TryGetComponent FoodData.ID) with
        | None -> Ok None
        | Some c -> tryRegrowFood c.ToFood
        
    override me.Initialize = 
        evm.RegisterListener "FoodSystem" Eaten me.onEaten
        evm.RegisterListener "FoodSystem" Food_AllEaten me.onAllEaten
        evm.RegisterListener "FoodSystem" PlantRegrowth me.onRegrowth
        base.SetToInitialized

    override me.Update = 
        ()


