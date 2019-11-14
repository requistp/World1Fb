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


type ControllerSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager
        
    let getCurrentActions actions entityID round = 
        let actionEnabledTest action = 
            match action with
            | Eat -> if EatingSystem.EatActionEnabled enm entityID then Some Eat else None
            | Mate -> if MatingSystem.MateActionEnabled enm entityID round then Some Mate else None
            | Move_North | Move_East | Move_South | Move_West ->
                let movesAllowed = MovementSystem.MovementActionsAllowed enm entityID
                match action with 
                | Move_North -> if movesAllowed |> Array.contains Move_North then Some Move_North else None
                | Move_East -> if movesAllowed |> Array.contains Move_East then Some Move_East else None
                | Move_South -> if movesAllowed |> Array.contains Move_South then Some Move_South else None
                | Move_West -> if movesAllowed |> Array.contains Move_West then Some Move_West else None
        actions 
        |> Array.Parallel.choose (fun action -> actionEnabledTest action)

    member private me.onSetActions round (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let actions = 
            let ects = 
                c.EntityID |> enm.GetComponents |> Array.Parallel.map (fun ct -> ct.ComponentID)
            let hasAllComponents required = 
                required |> Array.forall (fun ct -> ects |> Array.contains ct)
            ActionTypes.AsArray
            |> Array.Parallel.collect (fun action -> if hasAllComponents action.RequiredComponents then [|action|] else [||])
        
        match (ArrayContentsMatch actions c.Actions) with
        | true -> Ok None
        | false ->
            let current = getCurrentActions actions c.EntityID round
            enm.ReplaceComponent (Controller (c.Update (Some actions) (Some current)))
            Ok (Some (sprintf "Actions:%A. Current:%A" actions current))

    member private me.UpdateCurrentActionsForAllEntities round = 
        ControllerComponentID
        |> enm.GetEntitiesWithComponent 
        |> Array.Parallel.iter (fun eid -> 
            let c = (eid|>enm.GetComponent ControllerComponentID).ToController
            let newCurrent = getCurrentActions c.Actions c.EntityID round
            if not (ArrayContentsMatch newCurrent c.CurrentActions) then 
                enm.ReplaceComponent (Controller (c.Update None (Some newCurrent)))
                game.Logger.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "OK" (me.ToString) "Update current actions" c.EntityID newCurrent)
            )

    override me.Initialize = 
        evm.RegisterListener me.ToString Event_ComponentAdded_Controller_ID (me.TrackTask me.onSetActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override _.ToString = "ControllerSystem"

    override me.Update round = 
        me.UpdateCurrentActionsForAllEntities round

