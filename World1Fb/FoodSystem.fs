module FoodSystem
open AbstractSystem
open FoodComponent
open GameEvents
open GameManager
open SystemManager


type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(Sys_Food, isActive) 
    



        //game.EventManager.RegisterListener Eaten this.onEaten
    //member private this.onEaten (ge:AbstractGameEvent) =
    //    let e = ge :?> Event_Eaten
        
    //    this.ChangeLog.AddComponentChange (FoodComponent_Change(e.EateeID, -e.Quantity))

(*
        
//type FoodComponent_Change(eid:uint32, invalid:string option, quantity:int) =
//    inherit AbstractComponentChange(Food,eid,invalid)
        
//    member _.Quantity = quantity
        
//    override this.AddChange (a:AbstractComponent) =
//        let c = a :?> FoodComponent
//        FoodComponent(eid, c.FoodType, this.Quantity + c.Quantity) :> AbstractComponent
        
//    override this.Invalidate (reason:string) =
//        FoodComponent_Change(this.EntityID, Some reason, this.Quantity) :> AbstractComponentChange
        
//    new (eid:uint32, quantity:int) = FoodComponent_Change(eid, None, quantity)
        
*)