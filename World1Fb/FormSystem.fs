module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    //member private this.onMovement (ge:AbstractGameEvent) =
    //    let m = ge :?> Event_Movement
    //    this.ChangeLog.AddComponentChange (FormComponent_Change(m.EntityID, None, None, { X = m.Direction.X_change; Y = m.Direction.Y_change; Z = m.Direction.Z_change }))

    override this.Initialize = 
        //game.EventManager.RegisterListener Movement this.onMovement
        base.SetToInitialized

    override this.Update = 
        ()
