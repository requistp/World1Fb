module InputHandler
open ComponentEnums
open ControllerComponent
open agent_Entities
open EventManager
open EventTypes
open System


type KeyboardResult = 
| ExitGame
| GameAction
| InfoOnly


type InputHandler(evm:EventManager, enm:agent_Entities) =
    let mutable _entityID = None

    member _.GetEntityID = _entityID
    member _.SetEntityID eid = _entityID <- eid

    member _.AwaitKeyboardInput =
        let handleKeyPressed (k:ConsoleKeyInfo) = 
            let processAction (evm:EventManager) (action:ActionTypes) (entityID:uint32) = 
                match ControllerSystem.HandleAction enm evm action entityID with
                | false -> InfoOnly
                | true -> GameAction

            while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
                Console.ReadKey(true).Key |> ignore

            match k.Key with 
            | ConsoleKey.Escape -> ExitGame
            | ConsoleKey.E -> processAction evm Eat _entityID.Value
            | ConsoleKey.Spacebar -> processAction evm Idle _entityID.Value
            | ConsoleKey.M -> processAction evm Mate _entityID.Value
            | ConsoleKey.RightArrow -> processAction evm Move_East _entityID.Value
            | ConsoleKey.UpArrow -> processAction evm Move_North _entityID.Value
            | ConsoleKey.DownArrow -> processAction evm Move_South _entityID.Value
            | ConsoleKey.LeftArrow -> processAction evm Move_West _entityID.Value
            | _ -> InfoOnly

        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 2

        handleKeyPressed (Console.ReadKey(true))


