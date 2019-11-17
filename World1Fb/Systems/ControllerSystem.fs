module ControllerSystem
open agent_GameLog
open CommonGenericFunctions
open Component
open ComponentEnums
open ControllerComponent
open EatingSystem
open EntityExtensions
open EventManager
open EventTypes
open MatingSystem
open MovementSystem
open SystemManager
open EntityManager
open InputHandler

let private getCurrentActions (enm:EntityManager) (actions:ActionTypes[]) (entityID:uint32) (round:uint32) = 
    let movesAllowed = MovementActionsAllowed enm entityID
    let actionEnabledTest action =             
        match action with
        | Eat -> if EatActionEnabled enm entityID then Some Eat else None
        | Idle -> Some Idle
        | Mate -> if MateActionEnabled enm entityID round then Some Mate else None
        | Move_North -> if movesAllowed |> Array.contains Move_North then Some Move_North else None
        | Move_East ->  if movesAllowed |> Array.contains Move_East  then Some Move_East  else None
        | Move_South -> if movesAllowed |> Array.contains Move_South then Some Move_South else None
        | Move_West ->  if movesAllowed |> Array.contains Move_West  then Some Move_West  else None
    actions |> Array.Parallel.choose (fun action -> actionEnabledTest action)

let GetInputForAllEntities (enm:EntityManager) (log:agent_GameLog) (round:uint32) = 
    let setCurrentActions (controller:ControllerComponent) = 
        let newCurrent = getCurrentActions enm controller.PotentialActions controller.EntityID round
        match (ArrayContentsMatch newCurrent controller.CurrentActions) with
        | true -> controller
        | false ->
            let newController = controller.Update None None (Some newCurrent) None
            enm.ReplaceComponent (Controller newController)
            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current actions" controller.EntityID newCurrent)
            newController

    let getAIInputForEntity (controller:ControllerComponent) =
        let newAction =
            match controller.ControllerType with
            | AI_Random -> 
                controller.CurrentActions.[random.Next(controller.CurrentActions.Length)]
            | _ -> Idle // Should raise an error
        if (newAction <> controller.CurrentAction) then
            enm.ReplaceComponent (Controller (controller.Update None (Some newAction) None None))
            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current action:" controller.EntityID newAction)

    let getKeyboardInputForEntity (controller:ControllerComponent) =
        let newAction,cont =
            match controller.ControllerType with
            | Keyboard -> 
                AwaitKeyboardInput controller
                //(controller.CurrentActions.[random.Next(controller.CurrentActions.Length)], true)
            | _ -> Idle,false
        match cont with
        | false -> false
        | true -> 
            if (newAction <> controller.CurrentAction) then
                enm.ReplaceComponent (Controller (controller.Update None (Some newAction) None None))
                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current action:" controller.EntityID newAction)
            true

    let handleSplitInputTypes (keyboard:ControllerComponent[],ai:ControllerComponent[]) =
        ai 
            |> Array.Parallel.iter (fun c -> getAIInputForEntity c)

        keyboard 
            |> Array.map (fun c -> getKeyboardInputForEntity c)
            |> Array.forall (fun b -> b)

    ControllerComponentID
    |> EntityExt.GetEntitiesWithComponent enm 
    |> Array.Parallel.map (fun c -> setCurrentActions c.ToController)
    |> Array.Parallel.partition (fun c -> c.ControllerType = Keyboard)
    |> handleSplitInputTypes 

type ControllerSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
        
    let handleAction (controller:ControllerComponent) = 
        match controller.CurrentAction with
        | Idle -> ()
        | _ -> 
            evm.RaiseEvent (
                match controller.CurrentAction with 
                | Eat -> Action_Eat { EntityID = controller.EntityID }
                | Mate -> Action_Mate { EntityID = controller.EntityID }
                | Move_North -> Action_Movement { EntityID = controller.EntityID; Direction = North }
                | Move_East -> Action_Movement { EntityID = controller.EntityID; Direction = East }
                | Move_South -> Action_Movement { EntityID = controller.EntityID; Direction = South }
                | Move_West -> Action_Movement { EntityID = controller.EntityID; Direction = West }
                )

    member private me.onSetPotentialActions (round:uint32) (ge:GameEventTypes) =
        let c = ge.ToComponentAddedController.Component.ToController
        let potential = 
            let ects = EntityExt.GetComponentIDs enm c.EntityID
            ActionTypes.AsArray 
            |> Array.Parallel.choose (fun a -> if a.RequiredComponents |> Array.forall (fun ct -> ects |> Array.contains ct) then Some a else None)
        
        match (ArrayContentsMatch potential c.PotentialActions) with
        | true -> Ok None
        | false ->
            let current = getCurrentActions enm potential c.EntityID round
            enm.ReplaceComponent (Controller (c.Update None None (Some current) (Some potential)))
            Ok (Some (sprintf "Actions:%A. Current:%A" potential current))

    member private me.handleAllActions =
        ControllerComponentID
        |> EntityExt.GetEntitiesWithComponent enm 
        |> Array.Parallel.iter (fun c -> handleAction c.ToController)

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_Controller_ID (me.TrackTask me.onSetPotentialActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override me.Update round = 
        me.handleAllActions



        (*
        let newAction,cont =
            match controller.ControllerType with
            | AI_Random -> 
                (controller.CurrentActions.[random.Next(controller.CurrentActions.Length)], true)
            | Keyboard -> 
                AwaitKeyboardInput controller
                //(controller.CurrentActions.[random.Next(controller.CurrentActions.Length)], true)

        match cont with
        | false -> false
        | true -> 
            if (newAction <> controller.CurrentAction) then
                enm.ReplaceComponent (Controller (controller.Update None (Some newAction) None None))
                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current action:" controller.EntityID newAction)
            true
            *)