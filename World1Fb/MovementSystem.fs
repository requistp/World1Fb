﻿module MovementSystem
open AbstractComponent
open ControllerComponent
open EntityComponentManager
open EventManager
open FormComponent
open GameEvents
open GameManager
open LocationTypes
open MovementComponent
open System
open SystemManager

type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    let isMovementValid (eid:uint32) (md:MovementDirection) = 
        let checkDestinationOnMap (md:MovementDirection) (fc:FormComponent) =
            let dest = md.AddToLocation fc.Location
            match dest.IsOnMap with
            | false -> None
            | true -> Some (dest,fc)
        let checkTerrainIsPassable (dest:LocationDataInt) =
            true
        match eid |> Entity.TryGetComponent game.Entities Form with
        | None -> None
        | Some fco -> Some (fco :?> FormComponent)
        |> Option.bind (checkDestinationOnMap md)
        //|> Option.bind 
        |> Option.isSome

        //Terrain Is Passable

        //Form at location? is it and I not passable?

        //true

    let onMovementKeyPressed (ge:AbstractGameEvent) = 
        let m = ge :?> GameEvent_KeyPressed_Movement
        // I go through here because if the form listened for a movement keypress event, it could move without having a move component 
        // Also, maybe the entity is paralyzed, that would be filtered here and not trigger the movement event below
        if (isMovementValid m.EntityID m.Direction) then game.EventManager.QueueEvent(GameEvent_Movement(m.EntityID,m.Direction))

    override _.Initialize = 
        game.EventManager.RegisterListener Movement_KeyPressed onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        base.ChangeLog_PackageAndClose
