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

    let handleKeyPressed (k:ConsoleKeyInfo) = 
        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore

        let handleAction (action:ActionTypes) = 
            match (_entityID.Value |> enm.GetComponent ControllerComponentID).ToController.CurrentActions |> Array.contains action with
            | false -> InfoOnly
            | true when action = Idle -> GameAction
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
        | ConsoleKey.E -> handleAction Eat
        | ConsoleKey.Spacebar -> handleAction Idle
        | ConsoleKey.M -> handleAction Mate
        | ConsoleKey.RightArrow -> handleAction Move_East
        | ConsoleKey.UpArrow -> handleAction Move_North
        | ConsoleKey.DownArrow -> handleAction Move_South
        | ConsoleKey.LeftArrow -> handleAction Move_West
        | _ -> InfoOnly
        
    member _.GetEntityID = _entityID
    member _.SetEntityID eid = _entityID <- eid

    member me.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 2
        handleKeyPressed (Console.ReadKey(true))


