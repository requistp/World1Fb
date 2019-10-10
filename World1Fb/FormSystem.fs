module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open EntityComponentManager
open EventManager
open LocationTypes
open SystemManager

type FormSystem(game:Game, isActive:bool, initialForms:AbstractComponent[][]) =
    inherit AbstractSystem(isActive) 

    member private this.setInitialForms = 
        initialForms 
        |> Array.iter (fun ne -> this.ChangeLog_NewEntity ne)

    member private this.onMovement (ge:AbstractGameEvent) =
        let m = ge :?> GameEvent_Movement
        this.ChangeLog_AddComponentChange (FormComponent_Change(m.EntityID, None, None, { X=m.Direction.X_change; Y=m.Direction.Y_change }))

    override this.Initialize = 
        this.setInitialForms
        game.EventManager.RegisterListener Movement this.onMovement
        base.SetToInitialized

    override _.Update = 
        base.ChangeLog_PackageAndClose
