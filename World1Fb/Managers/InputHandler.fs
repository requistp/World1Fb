module InputHandler
open Component
open ComponentEnums
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

    member private me.HaveEntityAndRequiredComponents (cts:byte[]) =
        match _entityID with
        | None -> false
        | Some eid -> enm.HasAllComponents cts eid
    
    member private me.HandleAction (requiredComponents:byte[]) event =
        match me.HaveEntityAndRequiredComponents requiredComponents with
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

    member private me.onKeyPressed (k:ConsoleKeyInfo) = 
        match k.Key with 
        | ConsoleKey.UpArrow -> 
            let action() = evm.ExecuteEvent (Action_Movement { EntityID=_entityID.Value; Direction=North })
            me.HandleAction [| FormComponent.ID; MovementComponent.ID |] action
        | ConsoleKey.DownArrow -> 
            let action() = evm.ExecuteEvent (Action_Movement { EntityID=_entityID.Value; Direction=South })
            me.HandleAction [| FormComponent.ID; MovementComponent.ID |] action
        | ConsoleKey.LeftArrow -> 
            let action() = evm.ExecuteEvent (Action_Movement { EntityID=_entityID.Value; Direction=West })
            me.HandleAction [| FormComponent.ID; MovementComponent.ID |] action
        | ConsoleKey.RightArrow -> 
            let action() = evm.ExecuteEvent (Action_Movement { EntityID=_entityID.Value; Direction=East })
            me.HandleAction [| FormComponent.ID; MovementComponent.ID |] action
        | ConsoleKey.E -> 
            let action() = evm.ExecuteEvent (Action_Eat { EntityID=_entityID.Value })
            me.HandleAction [| EatingComponent.ID |] action
        | ConsoleKey.M -> 
            let action() = evm.ExecuteEvent (Action_Mate { EntityID=_entityID.Value })
            me.HandleAction [| MatingComponent.ID |] action
        | _ -> ()  

        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
        GameAction
        
    member me.SetEntityID eid = _entityID <- eid

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
               
