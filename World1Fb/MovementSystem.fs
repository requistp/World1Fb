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

    //member private this.onMovementKeyPressed (ge:AbstractGameEvent) = 
    //    let m = ge :?> Event_Action_Movement

    //    let isMovementValid = 
    //        let someIfDestinationOnMap (fc:FormComponent) =
    //             let dest = m.Direction.AddToLocation fc.Location
    //             match dest.IsOnMap with
    //             | false -> None
    //             | true -> Some (dest,fc)
        
    //        // This should only handle things that prevent movement within the context of the movement system (map size, being paralyzed, etc.)
    //        // Or maybe other systems can listen for this event, and mark the event invalid?

    //        match game.EntityManager.TryGetComponent Form m.EntityID with
    //        | None -> None
    //        | Some fc -> Some (fc :?> FormComponent)
    //        |> Option.bind someIfDestinationOnMap
    //        |> Option.isSome

    //    match isMovementValid with
    //    | false -> () // Record rejected event? Maybe some day
    //    | true -> game.EventManager.QueueEvent(Event_Movement(m.EntityID,m.Direction))

    override this.Initialize = 
        //game.EventManager.RegisterListener Action_Movement this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        ()
    