module MovementSystem
open AbstractComponent
open AbstractSystem
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
    inherit AbstractSystem(Sys_Movement, isActive) 

    member private this.onMovementKeyPressed (next:NextEntityDictionary) (ge:AbstractGameEvent) : Result<string option,string> =
        let m = ge :?> Event_Action_Movement

        let form = game.EntityManager.GetComponent<FormComponent> m.EntityID
        let dest = m.Direction.AddToLocation form.Location

        let isMovementValid = 
            let checkIfDestinationOnMap =
                 match dest.IsOnMap with
                 | false -> Error "onMovementKeyPressed: Destination is not on map"
                 | true -> Ok None
        
            let testForImpassableFormAtLocation junk =
                let formImpassableAtLocation =
                    dest
                    |> next.EntitiesAtLocation
                    |> Array.filter (fun e -> e <> m.EntityID)
                    |> Array.Parallel.map (fun eid -> next.GetComponent<FormComponent> eid)
                    |> Array.exists (fun f -> not f.IsPassable)
                match formImpassableAtLocation with
                | true -> Error "Something at location"
                | false -> Ok None

            checkIfDestinationOnMap
            |> Result.bind testForImpassableFormAtLocation

        match isMovementValid with
        | Error s -> Error s
        | Ok _ -> next.ReplaceComponent (FormComponent(m.EntityID, form.IsPassable, form.Name, form.Symbol, dest))
                
    override this.Initialize = 
        game.EventManager.RegisterListener Action_Movement this.onMovementKeyPressed
        base.SetToInitialized
