module ControllerSystem
open agent_GameLog
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EatingSystem
open EntityManager
open EventManager
open EventTypes
open MatingSystem
open MovementSystem
open SystemManager

let HandleAction (enm:EntityManager) (evm:EventManager) (action:ActionTypes) (entityID:uint32) = 
    match (entityID |> enm.GetComponent ControllerComponentID).ToController.CurrentActions |> Array.contains action with
    | false -> false
    | true when action = Idle -> true
    | true -> 
        evm.RaiseEvent (
            match action with 
            | Eat -> Action_Eat { EntityID = entityID }
            | Mate -> Action_Mate { EntityID = entityID }
            | Move_North -> Action_Movement { EntityID = entityID; Direction = North }
            | Move_East -> Action_Movement { EntityID = entityID; Direction = East }
            | Move_South -> Action_Movement { EntityID = entityID; Direction = South }
            | Move_West -> Action_Movement { EntityID = entityID; Direction = West }
            )
        true

let private getCurrentActions enm actions entityID round = 
    let movesAllowed = MovementSystem.MovementActionsAllowed enm entityID
    let actionEnabledTest action =             
        match action with
        | Eat -> if EatingSystem.EatActionEnabled enm entityID then Some Eat else None
        | Idle -> Some Idle
        | Mate -> if MatingSystem.MateActionEnabled enm entityID round then Some Mate else None
        | Move_North -> if movesAllowed |> Array.contains Move_North then Some Move_North else None
        | Move_East ->  if movesAllowed |> Array.contains Move_East  then Some Move_East  else None
        | Move_South -> if movesAllowed |> Array.contains Move_South then Some Move_South else None
        | Move_West ->  if movesAllowed |> Array.contains Move_West  then Some Move_West  else None
    actions |> Array.Parallel.choose (fun action -> actionEnabledTest action)

let private setCurrentActions (enm:EntityManager) (log:agent_GameLog) round entityID = 
    let c = (entityID|>enm.GetComponent ControllerComponentID).ToController
    let newCurrent = getCurrentActions enm c.Actions c.EntityID round
    if not (ArrayContentsMatch newCurrent c.CurrentActions) then
        enm.ReplaceComponent (Controller (c.Update None (Some newCurrent)))
        log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current actions" entityID newCurrent)

let UpdateCurrentActionsForAllEntities (enm:EntityManager) (log:agent_GameLog) round = 
    ControllerComponentID
    |> History.GetEntitiesWithComponent enm (Some round)
    |> Array.Parallel.iter (fun eid -> setCurrentActions enm log round eid)


type ControllerSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
        
    member private me.onSetActions round (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let actions = 
            let ects = enm.GetComponentIDs c.EntityID
            ActionTypes.AsArray 
            |> Array.Parallel.choose (fun a -> if a.RequiredComponents |> Array.forall (fun ct -> ects |> Array.contains ct) then Some a else None)
        
        match (ArrayContentsMatch actions c.Actions) with
        | true -> Ok None
        | false ->
            let current = getCurrentActions enm actions c.EntityID round
            enm.ReplaceComponent (Controller (c.Update (Some actions) (Some current)))
            Ok (Some (sprintf "Actions:%A. Current:%A" actions current))

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_Controller_ID (me.TrackTask me.onSetActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override me.Update round = 
        () 

