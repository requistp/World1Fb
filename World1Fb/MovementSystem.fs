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

    let isMovementValid (eid:uint32) (md:MovementDirection) = 
        let checkDestinationOnMap (md:MovementDirection) (fc:FormComponent) =
            let dest = md.AddToLocation fc.Location
            match dest.IsOnMap with
            | false -> None
            | true -> Some (dest,fc)
        let checkTerrainIsPassable (dest:LocationDataInt) =
            true
        match Form |> game.EntityManager.TryGetComponent eid with
        | None -> None
        | Some fco -> Some (fco :?> FormComponent)
        |> Option.bind (checkDestinationOnMap md)
        //|> Option.bind 
        |> Option.isSome

        //Terrain Is Passable

        //Form at location? is it and I not passable?

        //true

    member private this.onMovementKeyPressed (ge:AbstractGameEvent) = 
        let m = ge :?> Event_KeyPressed_Movement
        // I go through here because if the form listened for a movement keypress event, it could move without having a move component 
        // Also, maybe the entity is paralyzed, that would be filtered here and not trigger the movement event below
        match m.Direction|>isMovementValid m.EntityID with
        | false -> ()
        | true -> game.EventManager.QueueEvent(Event_Movement(m.EntityID,m.Direction))

    override this.Initialize = 
        game.EventManager.RegisterListener Movement_KeyPressed this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        base.ChangeLog_PackageAndClose
