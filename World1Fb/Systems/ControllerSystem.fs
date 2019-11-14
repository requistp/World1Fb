module ControllerSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EatingSystem
open EventTypes
open GameManager
open LocationTypes
open MatingSystem
open MovementSystem
open System
open SystemManager


type ControllerSystem(description:string, game:Game, isActive:bool) =
    inherit AbstractSystem(description,isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager
        
    let getCurrentActions actions entityID round = 
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
    
    member private me.setCurrentActions entityID round = 
        let c = (entityID|>enm.GetComponent ControllerComponentID).ToController
        let newCurrent = getCurrentActions c.Actions c.EntityID round
        match (ArrayContentsMatch newCurrent c.CurrentActions) with
        | true -> Ok None
        | false ->
            enm.ReplaceComponent (Controller (c.Update None (Some newCurrent)))
            Ok (Some (sprintf "Current actions: %A" newCurrent))        

    member private me.onSetCurrentActions round (ge:GameEventTypes) =
        me.setCurrentActions ge.EntityID round

    member private me.onSetActions round (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let actions = 
            let ects = 
                c.EntityID |> enm.GetComponents |> Array.Parallel.map (fun ct -> ct.ComponentID)
            let hasAllComponents required = 
                required |> Array.forall (fun ct -> ects |> Array.contains ct)
            ActionTypes.AsArray |> Array.Parallel.collect (fun a -> if hasAllComponents a.RequiredComponents then [|a|] else [||])
        
        match (ArrayContentsMatch actions c.Actions) with
        | true -> Ok None
        | false ->
            let current = getCurrentActions actions c.EntityID round
            enm.ReplaceComponent (Controller (c.Update (Some actions) (Some current)))
            Ok (Some (sprintf "Actions:%A. Current:%A" actions current))

    member me.UpdateCurrentActionsForAllEntities round = 
        ControllerComponentID
        |> enm.GetEntitiesWithComponent 
        |> Array.Parallel.iter (fun eid -> me.setCurrentActions eid round |> ignore)

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_Controller_ID (me.TrackTask me.onSetActions)
        evm.RegisterListener me.Description Event_LocationChanged_ID           (me.TrackTask me.onSetCurrentActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override me.Update round = 
        me.UpdateCurrentActionsForAllEntities round

