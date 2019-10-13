module MovementSystem
open AbstractComponent
open ControllerComponent
open EntityManager
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

    member private this.onMovementKeyPressed (ge:AbstractGameEvent) = 
        let m = ge :?> Event_KeyPressed_Movement

        let isMovementValid = 
            let someIfDestinationOnMap (fc:FormComponent) =
                let dest = m.Direction.AddToLocation fc.Location
                match dest.IsOnMap with
                | false -> None
                | true -> Some (dest,fc)

            let someIfTerrainIsPassable (dest:LocationDataInt, fc:FormComponent) =
                let impassableFormAtLocation = 
                    game.EntityManager.FormsAtLocation dest
                    |> Array.exists (fun f -> not f.IsPassable)
                match impassableFormAtLocation with
                | true -> None
                | false -> Some (dest,fc)

            match Form |> game.EntityManager.TryGetComponent m.EntityID with
            | None -> None
            | Some fco -> Some (fco :?> FormComponent)
            |> Option.bind someIfDestinationOnMap
            |> Option.bind someIfTerrainIsPassable 
            |> Option.isSome

        match isMovementValid with
        | false -> ()
        | true -> game.EventManager.QueueEvent(Event_Movement(m.EntityID,m.Direction))

    override this.Initialize = 
        game.EventManager.RegisterListener Movement_KeyPressed this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        this.ChangeLog.PackageAndClose

    
