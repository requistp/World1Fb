module EatingSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open SystemManager


type EatingSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onEat (ge:AbstractGameEvent) =
        let e = ge :?> Event_Eat
        // input handler or Eat_Command handler has already addressed the issue of what to eat
        ()
        //this.ChangeLog.AddComponentChange (FoodComponent_Change(e.EntityID, None, -e.Quantity))

    member private this.onKeyPressedEat (ge:AbstractGameEvent) =
        let e = ge :?> Event_KeyPressed_Eat

        //let foodAtLocation = 
        //    game.EntityManager.

        let aco = game.EntityManager.TryGetComponent Form e.EntityID

        match aco with
        | None -> ()
        | Some ac -> let fc = ac :?> FormComponent
                     ()

    override this.Initialize = 
        game.EventManager.RegisterListener Eat this.onEat
        game.EventManager.RegisterListener KeyPressed_Eat this.onKeyPressedEat
        base.SetToInitialized

    //override this.Update = 

