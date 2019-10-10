module InputHandler
open AbstractComponent
open EntityComponentManager
open EventManager
open GameEvents
open MovementComponent
open System


type InputHandler(em:EventManager) =
    let mutable _entityID = None

    let keyPressed_Movement d = 
        match _entityID with
        | None -> ()
        | Some eid -> em.QueueEvent(GameEvent_KeyPressed_Movement(eid,d))

    let onKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> keyPressed_Movement North 
        | ConsoleKey.DownArrow -> keyPressed_Movement South
        | ConsoleKey.LeftArrow -> keyPressed_Movement West 
        | ConsoleKey.RightArrow -> keyPressed_Movement East
        | _ -> ()  

        while Console.KeyAvailable do //Might helpclear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
            
    member _.SetEntityID eid = _entityID <- eid

    member _.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 250

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> onKeyPressed k
               true
