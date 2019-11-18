module MovementSystem
open Component
open ComponentEnums
open ControllerComponent
open EntityExtensions
open EventManager
open EventTypes
open SystemManager
open EntityManager

let MovementActionsAllowed (enm:EntityManager) (entityID:uint32) =
    let mutable _allowed = Array.empty<ActionTypes>
    let moveo = entityID|>EntityExt.TryGetComponent enm None MovementComponentID
    let location = entityID |> EntityExt.GetLocation enm
    let testOnMap (direction:MovementDirection) = (direction.AddToLocation location).IsOnMap
    let formImpassableAtLocation (direction:MovementDirection) =
        location
        |> direction.AddToLocation 
        |> EntityExt.GetEntitiesAtLocationWithComponent enm None FormComponentID (Some entityID)
        |> Array.exists (fun f -> not f.ToForm.IsPassable)
    match moveo.IsNone || moveo.Value.ToMovement.MovesPerTurn = 0 with 
    | true -> _allowed
    | false ->
        if (testOnMap North) && not (formImpassableAtLocation North) then _allowed <- Array.append _allowed [|Move_North|]
        if (testOnMap East)  && not (formImpassableAtLocation East)  then _allowed <- Array.append _allowed [|Move_East|]
        if (testOnMap South) && not (formImpassableAtLocation South) then _allowed <- Array.append _allowed [|Move_South|]
        if (testOnMap West)  && not (formImpassableAtLocation West)  then _allowed <- Array.append _allowed [|Move_West|]
        _allowed


//type MovementSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
//    inherit AbstractSystem(description,isActive)
  
//    member private me.onMovementKeyPressed round (ge:GameEventTypes) =
//        let e = ge.ToAction_Movement

//        let formo = EntityExt.TryGetComponent enm FormComponentID e.EntityID

//        let checkIfMovementIsValid (c:Component) = 
//            let form = c.ToForm
//            let dest = e.Direction.AddToLocation form.Location
//            let isMovementValid = 
//                let checkIfDestinationOnMap =
//                     match dest.IsOnMap with
//                     | false -> Error (sprintf "Not on map %s" (dest.ToString()))
//                     | true -> Ok None
//                let testForImpassableFormAtLocation junk =
//                    let formImpassableAtLocation =
//                        dest
//                        |> EntityExt.GetEntitiesAtLocationWithComponent enm FormComponentID (Some e.EntityID)
//                        |> Array.exists (fun f -> not f.ToForm.IsPassable)
//                    match formImpassableAtLocation with
//                    | true -> Error (sprintf "Form at location %s" (dest.ToString()))
//                    | false -> Ok None
//                checkIfDestinationOnMap
//                |> Result.bind testForImpassableFormAtLocation
//            match isMovementValid with
//            | Error s -> Error s
//            | Ok _ -> 
//                let f = Form (form.Update None None None (Some dest))
//                enm.UpdateComponent f
//                evm.RaiseEvent (LocationChanged { EntityID = e.EntityID; Form = f })
//                Ok (Some (sprintf "Location %s" (dest.ToString())))
//        match formo with
//        | None -> Error "Entity not in next dictionary"
//        | Some c -> checkIfMovementIsValid c

//    override me.Initialize = 
//        evm.RegisterListener me.Description Event_ActionMovement_ID (me.TrackTask me.onMovementKeyPressed)
//        base.SetToInitialized

//    override me.Update round = 
//        ()

