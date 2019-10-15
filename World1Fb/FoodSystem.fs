module FoodSystem
open AbstractComponent
open FoodComponent
open GameEvents
open GameManager
open SystemManager


type FoodSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onEaten (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eaten
        this.ChangeLog.AddComponentChange (FoodComponent_Change(e.EntityID, None, -e.Quantity))

    override this.Initialize = 
        game.EventManager.RegisterListener Eaten this.onEaten
        base.SetToInitialized

    //override this.Update = 

