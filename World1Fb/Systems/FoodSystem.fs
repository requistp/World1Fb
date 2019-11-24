module FoodSystem
open Component
open ComponentEnums
open EntityExtensions
open EventManager
open EventTypes
open FoodComponent
open System
open SystemManager
open EntityManager


type FoodSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
    
    member private me.onAllEaten round (Food_AllEaten (eat,food):GameEventData) =
        if (food.FoodType.KillOnAllEaten) then
            evm.RaiseEvent (Kill_AllEaten (eat,food))
        Ok None

    member private me.onEaten round (Eaten (eat,food):GameEventData) =
            let newQ = Math.Clamp(food.Quantity - eat.Quantity, 0, food.QuantityMax)
            let allEaten = newQ <= 0
            if allEaten then evm.RaiseEvent (Food_AllEaten (eat,food))
            enm.UpdateComponent round (Food (UpdateFood food None (Some newQ) None))
            Ok (Some (sprintf "Quantity:-%i=%i. All eaten:%b" eat.Quantity newQ allEaten))

    member private me.onRegrowth round (PlantRegrowth pg:GameEventData) =
        let tryRegrowFood (f:FoodComponent) = 
            let missing = f.QuantityMax - f.Quantity
            match (missing, pg.RegrowRate) with
            | (0,_) -> Ok (Some "Already maxed")
            | (_,0.0) -> Ok (Some "Zero regrow rate")
            | (_,_) -> 
                let quantity = Math.Clamp((int (Math.Round(pg.RegrowRate * (float f.QuantityMax),0))), 1, missing)
                enm.UpdateComponent round (Food (UpdateFood f None (Some (f.Quantity+quantity)) None)) 
                Ok (Some (sprintf "EntityID:%i. Regrown quantity:%i" pg.EntityID.ToUint32 quantity))
        match (EntityExt.TryGetComponent enm None FoodComponent pg.EntityID) with
        | None -> Ok None
        | Some (Food c) -> tryRegrowFood c
        | Some _ -> Error "Should not happen"
        
    override me.Initialize = 
        evm.RegisterListener me.Description Event_Eaten        (me.TrackTask me.onEaten)
        evm.RegisterListener me.Description Event_FoodAllEaten (me.TrackTask me.onAllEaten)
        evm.RegisterListener me.Description Event_PlantGrowth  (me.TrackTask me.onRegrowth)
        base.SetToInitialized

    override me.Update round = 
        ()


