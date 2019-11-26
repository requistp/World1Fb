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

let private getCurrentActions (enm:EntityManager) (actions:ActionTypes[]) (entityID:EntityID) (round:RoundNumber) = 
    let movesAllowed = MovementActionsAllowed enm entityID
    let actionEnabledTest action =             
        match action with
        | Eat -> if EatActionEnabled enm entityID then Some Eat else None
        | Idle -> Some Idle
        | Mate -> if MateActionEnabled enm entityID round then Some Mate else None
        | Move_North -> if Array.contains Move_North movesAllowed then Some Move_North else None
        | Move_East ->  if Array.contains Move_East  movesAllowed then Some Move_East  else None
        | Move_South -> if Array.contains Move_South movesAllowed then Some Move_South else None
        | Move_West ->  if Array.contains Move_West  movesAllowed then Some Move_West  else None
    actions |> Array.choose actionEnabledTest

let GetInputForAllEntities (enm:EntityManager) (log:agent_GameLog) (round:RoundNumber) (renderer:(EntityManager->EntityID->unit) option) = 
    let setCurrentActions (controller:ControllerComponent) = 
        let newCurrent = getCurrentActions enm controller.PotentialActions controller.EntityID round
        match (ArrayContentsMatch newCurrent controller.CurrentActions) with
        | true -> controller
        | false ->
            let newController = UpdateController controller None None (Some newCurrent) None
            enm.UpdateComponent (Controller newController)
            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current actions" controller.EntityID.ToUint32 newCurrent)
            newController

    let getAIInputForEntity (controller:ControllerComponent) =
        let newAction =
            match controller.ControllerType with
            | AI_Random -> 
                controller.CurrentActions.[random.Next(controller.CurrentActions.Length)]
            | _ -> Idle // Should raise an error
        if (newAction <> controller.CurrentAction) then
            enm.UpdateComponent (Controller (UpdateController controller None (Some newAction) None None))
            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %A" "Ok" "Controller System" "Current action" controller.EntityID.ToUint32 newAction)

    let getKeyboardInputForEntity (controller:ControllerComponent) =
        let newAction,cont =
            match controller.ControllerType with
            | Keyboard -> 
                AwaitKeyboardInput enm controller renderer round
            | _ -> Idle,false // Should raise an error
        match cont with
        | false -> false
        | true -> 
            if (newAction <> controller.CurrentAction) then
                enm.UpdateComponent (Controller (UpdateController controller None (Some newAction) None None))
                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i.%i : %A" "Ok" "Controller System" "Current action" controller.EntityID.ToUint32 controller.ID.ToUint32 newAction)
            true

    let handleSplitInputTypes (keyboard:ControllerComponent[],ai:ControllerComponent[]) =
        Async.Parallel
        (
            ai |> Array.Parallel.iter getAIInputForEntity
        )

        keyboard 
        |> Array.map getKeyboardInputForEntity
        |> Array.forall (fun b -> b)

    ControllerComponent
    |> enm.GetComponentsOfType
    |> Array.map (fun (Controller c) -> setCurrentActions c) 
    |> Array.partition (fun c -> c.ControllerType = Keyboard)
    |> handleSplitInputTypes 

type ControllerSystem(description:string, isActive:bool, enm:EntityManager, evm:EventManager) =
    inherit AbstractSystem(description,isActive) 
        
    let handleAction ((Controller controller):Component) = 
        match controller.CurrentAction with
        | Idle -> ()
        | _ -> 
            evm.RaiseEvent (
                match controller.CurrentAction with 
                | Eat  -> Action_Eat (ToEating (enm.GetComponent EatingComponent controller.EntityID))
                | Mate -> Action_Mate (ToMating (enm.GetComponent MatingComponent controller.EntityID))
                | Move_North -> Action_Movement ((ToForm (enm.GetComponent FormComponent controller.EntityID)), North)
                | Move_East  -> Action_Movement ((ToForm (enm.GetComponent FormComponent controller.EntityID)), East)
                | Move_South -> Action_Movement ((ToForm (enm.GetComponent FormComponent controller.EntityID)), South)
                | Move_West  -> Action_Movement ((ToForm (enm.GetComponent FormComponent controller.EntityID)), West)
                )

    member private me.onSetPotentialActions (round:RoundNumber) (ComponentAdded_Controller c:GameEventData) =
        let potential = 
            let ects = EntityExt.GetComponentTypes enm c.EntityID
            ActionTypes.AsArray 
            |> Array.choose (fun a -> if a.RequiredComponents |> Array.forall (fun ct -> ects |> Array.contains ct) then Some a else None)
        
        match (ArrayContentsMatch potential c.PotentialActions) with
        | true -> Ok None
        | false ->
            let current = getCurrentActions enm potential c.EntityID round
            enm.UpdateComponent (Controller (UpdateController c None None (Some current) (Some potential)))
            Ok (Some (sprintf "Actions:%A. Current:%A" potential current))

    member private me.handleAllActions =
        ControllerComponent
        |> enm.GetComponentsOfType
        |> Array.iter handleAction

    override me.Initialize = 
        evm.RegisterListener me.Description Event_ComponentAdded_Controller (me.TrackTask me.onSetPotentialActions)
        // Add Component
        // Remove Component
        base.SetToInitialized

    override me.Update round = 
        me.handleAllActions



