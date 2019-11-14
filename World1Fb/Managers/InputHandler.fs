module InputHandler
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

    member private me.handleKeyPressed (k:ConsoleKeyInfo) = 
        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore

        let handleAction (action:ActionTypes) = 
            match (_entityID.Value |> enm.GetComponent ControllerComponentID).ToController.CurrentActions |> Array.contains action with
            | false -> InfoOnly
            | true -> 
                evm.RaiseEvent (
                    match action with 
                    | Eat -> Action_Eat { EntityID = _entityID.Value }
                    | Mate -> Action_Mate { EntityID = _entityID.Value }
                    | Move_North -> Action_Movement { EntityID = _entityID.Value; Direction = North }
                    | Move_East -> Action_Movement { EntityID = _entityID.Value; Direction = East }
                    | Move_South -> Action_Movement { EntityID = _entityID.Value; Direction = South }
                    | Move_West -> Action_Movement { EntityID = _entityID.Value; Direction = West }
                    )
                GameAction

        match k.Key with 
        | ConsoleKey.Escape -> ExitGame
        | ConsoleKey.UpArrow -> handleAction Move_North
        | ConsoleKey.RightArrow -> handleAction Move_East
        | ConsoleKey.DownArrow -> handleAction Move_South
        | ConsoleKey.LeftArrow -> handleAction Move_West
        | ConsoleKey.E -> handleAction Eat
        | ConsoleKey.M -> handleAction Mate
        | ConsoleKey.Spacebar -> GameAction
        | _ -> InfoOnly
        
    member me.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 2
        me.handleKeyPressed (Console.ReadKey(true))


    //member me.SetDisplay k =
    //    match k with
    //    | ConsoleKey.F1 -> renderer_SetDisplay "World Map"
    //    | ConsoleKey.F2 -> 
    //        System.Console.SetCursorPosition(0,27)
    //        () //evm.PrintEventLog // renderer_SetDisplay "Game Events List"
    //    | _ -> ()
    //    InfoOnly