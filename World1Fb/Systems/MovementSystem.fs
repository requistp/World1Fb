module MovementSystem
open Component
open EventTypes
open GameManager
open SystemManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private me.onMovementKeyPressed (ge:GameEventTypes) =
        let e = ge.ToAction_Movement

        let formo = enm.TryGetComponent FormComponent.ID e.EntityID

        let checkIfMovementIsValid (c:Component) = 
            let form = c.ToForm
            let dest = e.Direction.AddToLocation form.Location
            let isMovementValid = 
                let checkIfDestinationOnMap =
                     match dest.IsOnMap with
                     | false -> Error (sprintf "Not on map %s" (dest.ToString()))
                     | true -> Ok None
                let testForImpassableFormAtLocation junk =
                    let formImpassableAtLocation =
                        dest
                        |> enm.GetEntitiesAtLocation
                        |> Array.filter (fun eid -> eid <> e.EntityID) // Not me
                        |> Array.Parallel.map (fun eid -> enm.GetComponent FormComponent.ID eid)
                        |> Array.exists (fun f -> not f.IsPassable)
                    match formImpassableAtLocation with
                    | true -> Error (sprintf "Form at location %s" (dest.ToString()))
                    | false -> Ok None
                checkIfDestinationOnMap
                |> Result.bind testForImpassableFormAtLocation
            match isMovementValid with
            | Error s -> Error s
            | Ok _ -> enm.ReplaceComponent (Form (form.Update None None None (Some dest))) (Some (sprintf "Location %s" (dest.ToString())))
        match formo with
        | None -> Error "Entity not in next dictionary"
        | Some c -> checkIfMovementIsValid c

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ActionMovement.ID me.onMovementKeyPressed
        base.SetToInitialized

    override _.ToString = "MovementSystem"

    override me.Update = 
        ()

