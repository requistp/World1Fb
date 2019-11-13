module InputHandler
open Component
open ComponentEnums
open ControllerComponent
open EntityManager
open EventManager
open EventTypes
open System

type KeyboardResult = 
    | ExitGame
    | GameAction
    | InfoOnly

type InputHandler(evm:EventManager, enm:EntityManager, renderer_SetDisplay:string->unit, wmrKeys:ConsoleKey->unit) =
    let mutable _entityID = None

    member _.GetEntityID = _entityID
    member _.SetEntityID eid = _entityID <- eid

    member me.SetDisplay k =
        match k with
        | ConsoleKey.F1 -> renderer_SetDisplay "World Map"
        | ConsoleKey.F2 -> 
            System.Console.SetCursorPosition(0,27)
            () //evm.PrintEventLog // renderer_SetDisplay "Game Events List"
        | _ -> ()
        InfoOnly

    member private me.onKeyPressed (k:ConsoleKeyInfo) = 
        let handleAction (ge:GameEventTypes) =
            match (_entityID.Value |> enm.GetComponent ControllerComponentID).ToController.CurrentActions |> Array.contains ge.ActionType.Value with
            | false -> ()
            | true -> evm.RaiseEvent ge

        match k.Key with 
        | ConsoleKey.UpArrow -> 
            handleAction (Action_Movement { EntityID = _entityID.Value; Direction = North })
        | ConsoleKey.DownArrow -> 
            handleAction (Action_Movement { EntityID = _entityID.Value; Direction = South })
        | ConsoleKey.LeftArrow -> 
            handleAction (Action_Movement { EntityID = _entityID.Value; Direction = West })
        | ConsoleKey.RightArrow -> 
            handleAction (Action_Movement { EntityID = _entityID.Value; Direction = East })
        | ConsoleKey.E -> 
            handleAction (Action_Eat { EntityID = _entityID.Value })
        | ConsoleKey.M -> 
            handleAction (Action_Mate { EntityID = _entityID.Value })
        | _ -> ()  

        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
        GameAction

        
    member me.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 1

        let k = Console.ReadKey(true)
        match k.Key with
        | ConsoleKey.Escape -> ExitGame
        | ConsoleKey.F1 -> me.SetDisplay k.Key
        | ConsoleKey.F2 -> me.SetDisplay k.Key
        | ConsoleKey.NumPad2 -> wmrKeys ConsoleKey.DownArrow; InfoOnly
        | ConsoleKey.NumPad4 -> wmrKeys ConsoleKey.LeftArrow; InfoOnly
        | ConsoleKey.NumPad6 -> wmrKeys ConsoleKey.RightArrow; InfoOnly
        | ConsoleKey.NumPad8 -> wmrKeys ConsoleKey.UpArrow; InfoOnly
        | _ -> me.onKeyPressed k
               


(*
member private me.HandleAction (action:ActionTypes) event =
    match _entityID with
    | None -> ()
    | Some eid -> 
        match ((eid|>enm.GetComponent ControllerComponentID).ToController.Actions |> Array.contains action) with
        | false -> ()
        | true -> event()

member private me.HaveEntityAndRequiredComponents (cts:byte[]) =
    match _entityID with
    | None -> false
    | Some eid -> enm.HasAllComponents cts eid

member private me.HandleAction (requiredComponents:byte[]) event =
    match me.HaveEntityAndRequiredComponents requiredComponents with
    | false -> ()
    | true -> event()
member private me.onKeyPressed (k:ConsoleKeyInfo) = 
    match k.Key with 
    | ConsoleKey.UpArrow -> 
        let action() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=North })
        me.HandleAction [| FormComponentID; MovementComponentID |] action
    | ConsoleKey.DownArrow -> 
        let action() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=South })
        me.HandleAction [| FormComponentID; MovementComponentID |] action
    | ConsoleKey.LeftArrow -> 
        let action() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=West })
        me.HandleAction [| FormComponentID; MovementComponentID |] action
    | ConsoleKey.RightArrow -> 
        let action() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=East })
        me.HandleAction [| FormComponentID; MovementComponentID |] action
    | ConsoleKey.E -> 
        let action() = evm.RaiseEvent (Action_Eat { EntityID=_entityID.Value })
        me.HandleAction [| EatingComponentID |] action
    | ConsoleKey.M -> 
        let action() = evm.RaiseEvent (Action_Mate { EntityID=_entityID.Value })
        me.HandleAction [| MatingComponentID |] action
    | _ -> ()  
    while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
        Console.ReadKey(true).Key |> ignore
    GameAction
*)