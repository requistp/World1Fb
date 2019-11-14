module MovementSystem
open Component
open ComponentEnums
open ControllerComponent
open EntityManager
open EventTypes
open GameManager
open SystemManager


type MovementSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    
  
    static member MovementActionsAllowed (enm:EntityManager) (entityID:uint32) =
        let mutable _allowed = Array.empty<ActionTypes>
        let move = (entityID|>enm.GetComponent MovementComponentID).ToMovement
        let location = enm.GetLocation entityID
        let testOnMap (direction:MovementDirection) = (direction.AddToLocation location).IsOnMap
        let formImpassableAtLocation (direction:MovementDirection) =
            location
            |> direction.AddToLocation 
            |> enm.GetEntitiesAtLocation
            |> Array.filter (fun eid -> eid <> entityID) // Not me
            |> Array.Parallel.map (fun eid -> eid|>enm.GetComponent FormComponentID)
            |> Array.exists (fun f -> not f.ToForm.IsPassable)
        match move.MovesPerTurn with 
        | 0 -> _allowed
        | _ ->
            if (testOnMap North) && not (formImpassableAtLocation North) then _allowed <- Array.append _allowed [|Move_North|]
            if (testOnMap East)  && not (formImpassableAtLocation East)  then _allowed <- Array.append _allowed [|Move_East|]
            if (testOnMap South) && not (formImpassableAtLocation South) then _allowed <- Array.append _allowed [|Move_South|]
            if (testOnMap West)  && not (formImpassableAtLocation West)  then _allowed <- Array.append _allowed [|Move_West|]
            _allowed
  
    member private me.onMovementKeyPressed round (ge:GameEventTypes) =
        let e = ge.ToAction_Movement

        let formo = enm.TryGetComponent FormComponentID e.EntityID

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
                        |> Array.Parallel.map (fun eid -> eid|>enm.GetComponent FormComponentID)
                        |> Array.exists (fun f -> not f.ToForm.IsPassable)
                    match formImpassableAtLocation with
                    | true -> Error (sprintf "Form at location %s" (dest.ToString()))
                    | false -> Ok None
                checkIfDestinationOnMap
                |> Result.bind testForImpassableFormAtLocation
            match isMovementValid with
            | Error s -> Error s
            | Ok _ -> 
                let f = Form (form.Update None None None (Some dest))
                enm.ReplaceComponent f
                evm.RaiseEvent (LocationChanged { EntityID = e.EntityID; Form = f })
                Ok (Some (sprintf "Location %s" (dest.ToString())))
        match formo with
        | None -> Error "Entity not in next dictionary"
        | Some c -> checkIfMovementIsValid c

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ActionMovement_ID (me.TrackTask me.onMovementKeyPressed)
        base.SetToInitialized

    override _.ToString = "MovementSystem"

    override me.Update round = 
        ()

