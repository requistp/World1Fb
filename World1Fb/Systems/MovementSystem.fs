module MovementSystem
open AbstractSystem
open EntityDictionary
open FormComponent
open EventTypes
open GameManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onMovementKeyPressed (next:NextEntityDictionary) (ge:EventData_Generic) =
        let m = ge :?> EventData_Action_Movement

        let formo = next.TryGetComponent<FormComponent> m.EntityID

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
                        |> next.EntitiesAtLocation
                        |> Array.filter (fun e -> e <> m.EntityID)
                        |> Array.Parallel.map (fun eid -> next.GetComponent<FormComponent> eid)
                        |> Array.exists (fun f -> not f.IsPassable)
                    match formImpassableAtLocation with
                    | true -> Error (sprintf "Form at location %s" dest.Print)
                    | false -> Ok None
                checkIfDestinationOnMap
                |> Result.bind testForImpassableFormAtLocation

            match isMovementValid with
            | Error s -> Error s
            | Ok _ -> next.ReplaceComponent (form.Update None None None (Some dest)) (Some (sprintf "Location %s" dest.Print))

        match formo with
        | None -> Error "Entity not in next dictionary"
        | Some f -> checkIfMovementIsValid f

    override this.Initialize = 
        game.EventManager.RegisterListener Action_Movement this.onMovementKeyPressed
        base.SetToInitialized
