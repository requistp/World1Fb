module MovementSystem
open AbstractComponent
open ControllerComponent
open EntityComponentManager
open EventManager
open FormComponent
open GameEvents
open GameManager
open MovementComponent
open System
open SystemManager

type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    let onMovementKeyPressed (ge:AbstractGameEvent) = 
        let m = ge :?> GameEvent_KeyPressed_Movement
        // I go through here because if the form listened for a movement keypress event, it could move without having a move component 
        // Also, maybe the entity is paralyzed, that would be filtered here and not trigger the movement event below
        game.EventManager.QueueEvent(GameEvent_Movement(m.EntityID,m.Direction))

    override _.Initialize = 
        base.SetToInitialized
        game.EventManager.RegisterListener Movement_KeyPressed onMovementKeyPressed
        ()

    override this.Update = 
        base.PackageAndCloseChangeLog
