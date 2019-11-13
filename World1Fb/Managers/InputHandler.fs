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

    member private me.HaveEntityAndRequiredComponents (cts:byte[]) =
        match _entityID with
        | None -> false
        | Some eid -> enm.HasAllComponents cts eid
    
    member private me.HandleAction (requiredComponents:byte[]) event =
        match me.HaveEntityAndRequiredComponents requiredComponents with
        | false -> ()
        | true -> event()

    member private me.HandleAction2 (action:ActionTypes) event =
        match _entityID with
        | None -> ()
        | Some eid -> 
            match ((eid|>enm.GetComponent ControllerComponentID).ToController.Actions |> Array.contains action) with
            | false -> ()
            | true -> event()

    member me.SetDisplay k =
        match k with
        | ConsoleKey.F1 -> renderer_SetDisplay "World Map"
        | ConsoleKey.F2 -> 
            System.Console.SetCursorPosition(0,27)
            () //evm.PrintEventLog // renderer_SetDisplay "Game Events List"
        | _ -> ()
        InfoOnly

    member private me.onKeyPressed2 (k:ConsoleKeyInfo) = 
        match k.Key with 
        | ConsoleKey.UpArrow -> 
            let event() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=North })
            me.HandleAction2 Move event
        | ConsoleKey.DownArrow -> 
            let event() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=South })
            me.HandleAction2 Move event
        | ConsoleKey.LeftArrow -> 
            let event() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=West })
            me.HandleAction2 Move event
        | ConsoleKey.RightArrow -> 
            let event() = evm.RaiseEvent (Action_Movement { EntityID=_entityID.Value; Direction=East })
            me.HandleAction2 Move event
        | ConsoleKey.E -> 
            let event() = evm.RaiseEvent (Action_Eat { EntityID=_entityID.Value })
            me.HandleAction2 Eat event
        | ConsoleKey.M -> 
            let event() = evm.RaiseEvent (Action_Mate { EntityID=_entityID.Value })
            me.HandleAction2 Mate event
        | _ -> ()  

        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
        GameAction

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
        | _ -> me.onKeyPressed2 k
               
