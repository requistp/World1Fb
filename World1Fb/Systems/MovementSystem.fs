module MovementSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EntityExtensions
open EventManager
open EventTypes
open SystemManager
open EntityManager

let MovementActionsAllowed (enm:EntityManager) (entityID:EntityID) =
    let mutable _allowed = Array.empty<ActionTypes>
    let (Component.Movement move) = enm.GetComponent None MovementComponentID entityID
    let location = EntityExt.GetLocation enm None entityID
    let testOnMap (direction:MovementDirection) = (direction.AddToLocation location).IsOnMap
    let formImpassableAtLocation (direction:MovementDirection) =
        location
        |> direction.AddToLocation 
        |> EntityExt.FormImpassableAtLocation enm None (Some entityID) 
    match move.MovesPerTurn = 0 with 
    | true -> _allowed
    | false ->
        if (testOnMap North) && not (formImpassableAtLocation North) then _allowed <- Array.append _allowed [|Move_North|]
        if (testOnMap East)  && not (formImpassableAtLocation East)  then _allowed <- Array.append _allowed [|Move_East|]
        if (testOnMap South) && not (formImpassableAtLocation South) then _allowed <- Array.append _allowed [|Move_South|]
        if (testOnMap West)  && not (formImpassableAtLocation West)  then _allowed <- Array.append _allowed [|Move_West|]
        _allowed


type MovementSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive)
  
    member private me.onMovementKeyPressed (round:RoundNumber) (ge:GameEventTypes) =
        let e = ge.ToAction_Movement
        let (Form form) = enm.GetComponent None FormComponentID e.EntityID
        let destination = e.Direction.AddToLocation form.Location
            
        match EntityExt.FormImpassableAtLocation enm None (Some e.EntityID) destination with
        | true -> Error (sprintf "Form at location %s" (destination.ToString()))
        | false -> 
            let f = form.Update None None None (Some destination)
            enm.UpdateComponent round (Form f)
            evm.RaiseEvent (LocationChanged { EntityID = e.EntityID; Form = f })
            Ok (Some (sprintf "Location %s" (destination.ToString())))
        
    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionMovement_ID (me.TrackTask me.onMovementKeyPressed)
        base.SetToInitialized

    override me.Update round = 
        ()

