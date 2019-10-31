module MovementSystem
open AbstractSystem
open Component
open EventTypes
open GameManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    member private this.onMovementKeyPressed (ge:EventData_Generic) =
        let m = ge :?> EventData_Action_Movement

        let formo = enm.TryGetComponent FormData.ID m.EntityID

        let checkIfMovementIsValid (c:Component) = 
            let (Form form) = c
            let dest = m.Direction.AddToLocation form.Location
            let isMovementValid = 
                let checkIfDestinationOnMap =
                     match dest.IsOnMap with
                     | false -> Error (sprintf "Not on map %s" (dest.ToString()))
                     | true -> Ok None
                let testForImpassableFormAtLocation junk =
                    let formImpassableAtLocation =
                        dest
                        |> enm.GetEntitiesAtLocation
                        |> Array.filter (fun e -> e <> m.EntityID)
                        |> Array.Parallel.map (fun eid -> enm.GetComponent FormData.ID eid)
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

    override this.Initialize = 
        evm.RegisterListener "MovementSystem" Action_Movement this.onMovementKeyPressed
        base.SetToInitialized

    override this.Update = 
        ()

