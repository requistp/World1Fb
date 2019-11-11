module FoodSystem
open Component
open ComponentEnums
open EventTypes
open FoodComponent
open GameManager
open System
open SystemManager


type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
    
    member private me.onAllEaten round (ge:GameEventTypes) =
        let e = ge.ToFoodAllEaten
        match (e.EateeID |> enm.GetComponent FoodComponentID).ToFood.FoodType.KillOnAllEaten with
        | false -> Ok None
        | true -> 
            evm.RaiseEvent (Kill_AllEaten { EaterID=e.EaterID; EateeID=e.EateeID })
            Ok None

    member private me.onEaten round (ge:GameEventTypes) =
        let e = ge.ToEaten        
        match enm.TryGetComponent FoodComponentID e.EateeID with
        | None -> Error "Something else ate it first"
        | Some c -> 
            let f = c.ToFood
            let newQ = Math.Clamp(f.Quantity - e.Quantity, 0, f.QuantityMax)
            let allEaten = newQ <= 0
            if allEaten then evm.RaiseEvent (Food_AllEaten { EaterID=e.EaterID; EateeID=e.EateeID })
            enm.ReplaceComponent (Food (f.Update None (Some newQ) None))
            Ok (Some (sprintf "Quantity:-%i=%i. All eaten:%b" e.Quantity newQ allEaten))

    member private me.onRegrowth round (ge:GameEventTypes) =
        let e = ge.ToPlantRegrowth
        let tryRegrowFood (f:FoodComponent) = 
            let pg = (e.EntityID|>enm.GetComponent PlantGrowthComponentID).ToPlantGrowth
            let missing = f.QuantityMax - f.Quantity
            match (missing, pg.RegrowRate) with
            | (0,_) -> Ok (Some "Already maxed")
            | (_,0.0) -> Ok (Some "Zero regrow rate")
            | (_,_) -> 
                let quantity = Math.Clamp((int (Math.Round(pg.RegrowRate * (float f.QuantityMax),0))), 1, missing)
                enm.ReplaceComponent (Food (f.Update None (Some (f.Quantity+quantity)) None)) 
                Ok (Some (sprintf "EntityID:%i. Regrown quantity:%i" e.EntityID quantity))
        match (e.EntityID|>enm.TryGetComponent FoodComponentID) with
        | None -> Ok None
        | Some c -> tryRegrowFood c.ToFood
        
    override me.Initialize = 
        evm.RegisterListener me.ToString Event_Eaten_ID        me.onEaten
        evm.RegisterListener me.ToString Event_FoodAllEaten_ID me.onAllEaten
        evm.RegisterListener me.ToString Event_PlantGrowth_ID  me.onRegrowth
        base.SetToInitialized

    override _.ToString = "FoodSystem"

    override me.Update round = 
        ()


