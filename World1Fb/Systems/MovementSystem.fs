module MovementSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EntityExtensions
open EntityManager
open EventManager
open EventTypes
open FormComponent
open LocationTypes
open SystemManager

let MovementActionsAllowed (enm:EntityManager) (entityID:EntityID) =
    let mutable _allowed = Array.empty<ActionTypes>
    let (Component.Movement move) = enm.GetComponent MovementComponent entityID
    let location = EntityExt.GetLocation enm entityID
    let testOnMap (direction:MovementDirection) = IsOnMap2D (direction.AddToLocation location)
    let formImpassableAtLocation (direction:MovementDirection) =
        location
        |> direction.AddToLocation 
        |> EntityExt.FormImpassableAtLocation enm (Some entityID) 
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
  
    member private me.onMovementKeyPressed (round:RoundNumber) (Action_Movement (f,d):GameEventData) =
        let destination = d.AddToLocation f.Location
            
        match not (IsOnMap2D destination) || EntityExt.FormImpassableAtLocation enm (Some f.EntityID) destination with
        | true -> Error (sprintf "Off map or form at location %s" (destination.ToString()))
        | false -> 
            let newForm = { f with Location = destination }
            enm.UpdateComponent (Form newForm)
            evm.RaiseEvent (LocationChanged newForm)
            Ok (Some (sprintf "Location %s" (destination.ToString())))
        
    override me.Initialize = 
        evm.RegisterListener me.Description Event_ActionMovement (me.TrackTask me.onMovementKeyPressed)
        base.SetToInitialized

    override me.Update round = 
        ()

