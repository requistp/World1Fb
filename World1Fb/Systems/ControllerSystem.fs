module ControllerSystem
open CommonGenericFunctions
open Component
open ControllerComponent
open EventTypes
open GameManager
open SystemManager


type ControllerSystem(game:Game, isActive:bool) =
    inherit AbstractSystem(isActive) 
    let enm = game.EntityManager
    let evm = game.EventManager    

    let availableActions eid = 
        let ects = 
            eid
            |> enm.GetComponents 
            |> Array.Parallel.map (fun ct -> ct.ComponentID)
        let hasAllComponents required = 
            required 
            |> Array.forall (fun ct -> ects |> Array.contains ct)
        ActionTypes.AsArray
        |> Array.Parallel.collect (fun a -> if hasAllComponents a.RequiredComponents then [|a|] else [||])

    member private me.onSetControllerActions round (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let actions = availableActions c.EntityID
        if not (ArrayContentsMatch actions c.Actions) then enm.ReplaceComponent (Controller (c.Update (Some actions)))
        Ok None

    override me.Initialize = 
        //evm.RegisterListener me.ToString Event_ComponentAdded_Controller_ID (me.TrackTask me.onSetControllerActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override _.ToString = "ControllerSystem"

    override me.Update round = 
        ()

