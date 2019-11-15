module MovementSystem
open Component
open ComponentEnums
open ControllerComponent
open EntityManager
open EventManager
open EventTypes
open SystemManager
open Entities

type MovementSystem(description:string, isActive:bool, enm:Entities, evm:EventManager) =
    inherit AbstractSystem(description,isActive)
  
    static member MovementActionsAllowed (enm:Entities) (entityID:uint32) =
        let mutable _allowed = Array.empty<ActionTypes>
        let moveo = entityID|>Entities.TryGetComponent enm MovementComponentID
        let location = entityID |> Entities.GetLocation enm
        let testOnMap (direction:MovementDirection) = (direction.AddToLocation location).IsOnMap
        let formImpassableAtLocation (direction:MovementDirection) =
            location
            |> direction.AddToLocation 
            |> Entities.GetEntitiesAtLocationWithComponent enm FormComponentID (Some entityID)
            |> Array.exists (fun f -> not f.ToForm.IsPassable)

        match moveo.IsNone || moveo.Value.ToMovement.MovesPerTurn = 0 with 
        | true -> _allowed
        | false ->
            if (testOnMap North) && not (formImpassableAtLocation North) then _allowed <- Array.append _allowed [|Move_North|]
            if (testOnMap East)  && not (formImpassableAtLocation East)  then _allowed <- Array.append _allowed [|Move_East|]
            if (testOnMap South) && not (formImpassableAtLocation South) then _allowed <- Array.append _allowed [|Move_South|]
            if (testOnMap West)  && not (formImpassableAtLocation West)  then _allowed <- Array.append _allowed [|Move_West|]
            _allowed
  
    member private me.onMovementKeyPressed round (ge:GameEventTypes) =
        let e = ge.ToAction_Movement

        let formo = Entities.TryGetComponent enm FormComponentID e.EntityID

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
                        |> Entities.GetEntitiesAtLocationWithComponent enm FormComponentID (Some e.EntityID)
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
        evm.RegisterListener me.Description Event_ActionMovement_ID (me.TrackTask me.onMovementKeyPressed)
        base.SetToInitialized

    override me.Update round = 
        ()

//game.Logger.Log round (sprintf "%5s | %5b | %s" "North" (testOnMap North) (location.ToString()))
//game.Logger.Log round (sprintf "%5s | %5b | %s" "East" (testOnMap East ) (location.ToString()))
//game.Logger.Log round (sprintf "%5s | %5b | %s" "South" (testOnMap South) (location.ToString()))
//game.Logger.Log round (sprintf "%5s | %5b | %s" "West" (testOnMap West ) (location.ToString()))