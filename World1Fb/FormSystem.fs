module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open EntityComponentManager
open EventManager
open LocationTypes
open SystemManager

type FormSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onMovement (ge:AbstractGameEvent) =
        let m = ge :?> GameEvent_Movement
        this.AppendChange (FormComponent_Change(m.EntityID, None, None, { X=m.Direction.X_change; Y=m.Direction.Y_change }))

    override this.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener Movement this.onMovement

    override this.Update = 
        this.ConsolidateChanges
