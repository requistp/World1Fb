module MovementSystem
open AbstractSystem
open EntityDictionary
open FormComponent
open GameEvents
open GameManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 

    member private this.onMovementKeyPressed (next:NextEntityDictionary) (ge:AbstractGameEvent) : Result<string option,string> =
        let m = ge :?> Event_Action_Movement

        let form = game.EntityManager.GetComponent<FormComponent> m.EntityID
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
        
        let result = sprintf "Location %s" dest.Print

        match isMovementValid with
        | Error s -> Error s
        | Ok _ -> next.ReplaceComponent (FormComponent(m.EntityID, form.IsPassable, form.Name, form.Symbol, dest)) (Some result)
                
    override this.Initialize = 
        game.EventManager.RegisterListener Action_Movement this.onMovementKeyPressed
        base.SetToInitialized
