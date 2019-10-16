module MovementSystem
open AbstractComponent
open ControllerComponent
open EntityDictionary
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


    member private this.onMovementKeyPressed (next:NextEntityDictionary) (ge:AbstractGameEvent) : Result<string option,string>= 
        let m = ge :?> Event_Action_Movement

        let form = (game.EntityManager.GetComponent Form m.EntityID) :?> FormComponent
        let dest = m.Direction.AddToLocation form.Location

        let isMovementValid = 
            let checkIfDestinationOnMap =
                 match dest.IsOnMap with
                 | false -> Error "onMovementKeyPressed: Destination is not on map"
                 | true -> Ok None
        
            //let testForImpassableFormAtLocation z =
            //    let formImpassableAtLocation (l:LocationDataInt) =
            //        l
            //        |> this.EntitiesAtLocation
            //        |> Array.Parallel.map (fun eid -> (this.GetComponent Form eid) :?> FormComponent)
            //        |> Array.exists (fun f -> not f.IsPassable)
            //    match newc.ComponentType=Form && formImpassableAtLocation (newc:?>FormComponent).Location with
            //    | true -> Some "Object at location"
            //    | false -> None

            checkIfDestinationOnMap
            //|> Result.bind 


        match isMovementValid with
        | Error s -> Error s
        | Ok s -> this.doMovement next (FormComponent(m.EntityID, form.IsPassable, form.Name, form.Symbol, dest))

        member private this.doMovement (next:NextEntityDictionary) (f:AbstractComponent) =
            next.ReplaceComponent f.EntityID f


    override this.Initialize = 
        game.EventManager.RegisterListener Action_Movement this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        ()
    