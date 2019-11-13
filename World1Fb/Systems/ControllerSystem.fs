module ControllerSystem
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EventTypes
open GameManager
open LocationTypes
open System
open SystemManager


type ControllerSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    let availableActions eid = 
        let ects = 
            eid |> enm.GetComponents |> Array.Parallel.map (fun ct -> ct.ComponentID)
        let hasAllComponents required = 
            required |> Array.forall (fun ct -> ects |> Array.contains ct)
        ActionTypes.AsArray
        |> Array.Parallel.collect (fun action -> if hasAllComponents action.RequiredComponents then [|action|] else [||])

    let getCurrentActions (c:ControllerComponent) = 
        c.Actions 
        |> Array.filter (fun action -> 
            match action with
            | Eat -> true  //Should I move some of my checking code here?
            | Mate -> true //Should I move some of my checking code here?
            | _ -> true
            )

    member private me.onSetActions round (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let actions = availableActions c.EntityID
        if not (ArrayContentsMatch actions c.Actions) then 
            enm.ReplaceComponent (Controller (c.Update (Some actions) (Some (getCurrentActions c))))
            game.Logger.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "OK" (me.ToString) "Update actions" c.EntityID actions)
        Ok None

    member private me.UpdateCurrentActionsForAllEntities round = 
        Console.SetCursorPosition(0,MapHeight)
        enm.GetEntitiesWithComponent ControllerComponentID
        |> Array.Parallel.iter (fun eid -> 
            let c = (eid|>enm.GetComponent ControllerComponentID).ToController
            let newCurrent = getCurrentActions c
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

