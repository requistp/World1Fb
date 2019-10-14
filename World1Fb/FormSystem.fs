module FormSystem
open AbstractComponent
open FormComponent
open GameEvents
open GameManager
open LocationTypes
open SystemManager


type FormSystem(game:Game, isActive:bool, initialForms:AbstractComponent[][]) =
    inherit AbstractSystem(isActive) 

    member private this.setInitialForms = 
        initialForms 
        |> Array.Parallel.iter (fun ne -> this.ChangeLog.NewEntity ne)

    member private this.onMovement (ge:AbstractGameEvent) =
        let m = ge :?> Event_Movement
        this.ChangeLog.AddComponentChange (FormComponent_Change(m.EntityID, None, None, None, { X = m.Direction.X_change; Y = m.Direction.Y_change; Z = m.Direction.Z_change }))

    override this.Initialize = 
        this.setInitialForms
        game.EventManager.RegisterListener Movement this.onMovement
        base.SetToInitialized

    override this.Update = 
        this.ChangeLog.PackageAndClose
