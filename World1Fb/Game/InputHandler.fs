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


type InputHandler(evm:EventManager, enm:EntityManager) =
    let mutable _entityID = None

    member _.GetEntityID = _entityID
    member _.SetEntityID eid = _entityID <- eid

    member _.AwaitKeyboardInput =
        let handleKeyPressed (k:ConsoleKeyInfo) = 
            let processAction (action:ActionTypes) (entityID:uint32) = 
                match ControllerSystem.HandleAction enm evm action entityID with
                | false -> InfoOnly
                | true -> GameAction

            while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
                Console.ReadKey(true).Key |> ignore

            match k.Key with 
            | ConsoleKey.Escape -> ExitGame
            | ConsoleKey.E -> processAction Eat _entityID.Value
            | ConsoleKey.Spacebar -> processAction Idle _entityID.Value
            | ConsoleKey.M -> processAction Mate _entityID.Value
            | ConsoleKey.RightArrow -> processAction Move_East _entityID.Value
            | ConsoleKey.UpArrow -> processAction Move_North _entityID.Value
            | ConsoleKey.DownArrow -> processAction Move_South _entityID.Value
            | ConsoleKey.LeftArrow -> processAction Move_West _entityID.Value
            | _ -> InfoOnly

        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 2

        handleKeyPressed (Console.ReadKey(true))


