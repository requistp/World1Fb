module MovementSystem
open AbstractSystem
open EntityManager
open FormComponent
open EventTypes
open GameManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private this.onMovementKeyPressed (ge:EventData_Generic) =
        let m = ge :?> EventData_Action_Movement

        let formo = enm.TryGetComponent<FormComponent> m.EntityID

        let checkIfMovementIsValid (form:FormComponent) = 
            let dest = m.Direction.AddToLocation form.Location
            let isMovementValid = 
                let checkIfDestinationOnMap =
                     match dest.IsOnMap with
                     | false -> Error (sprintf "Not on map %s" dest.Print)
                     | true -> Ok None
                let testForImpassableFormAtLocation junk =
                    let formImpassableAtLocation =
                        dest
                        |> enm.GetEntitiesAtLocation
                        |> Array.filter (fun e -> e <> m.EntityID)
                        |> Array.Parallel.map (fun eid -> enm.GetComponent<FormComponent> eid)
                        |> Array.exists (fun f -> not f.IsPassable)
                    match formImpassableAtLocation with
                    | true -> Error (sprintf "Form at location %s" dest.Print)
                    | false -> Ok None
                checkIfDestinationOnMap
                |> Result.bind testForImpassableFormAtLocation

            match isMovementValid with
            | Error s -> Error s
            | Ok _ -> enm.ReplaceComponent (form.Update None None None (Some dest)) (Some (sprintf "Location %s" dest.Print))

        match formo with
        | None -> Error "Entity not in next dictionary"
        | Some f -> checkIfMovementIsValid f

    override this.Initialize = 
        evm.RegisterListener "MovementSystem" Action_Movement this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        ()

