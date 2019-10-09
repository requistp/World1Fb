module InputHandler
open AbstractComponent
open EntityComponentManager
open EventManager
open GameEvents
open MovementComponent
open System


type InputHandler(em:EventManager, ecd:EntityComponentData) =

    let keyPressed_Movement d = 
        match Controller |> Entity.AllWithComponent ecd with
        | [] -> ()
        | l -> em.QueueEvent(GameEvent_KeyPressed_Movement(l.Head,d))

    let onKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> keyPressed_Movement North 
        | ConsoleKey.DownArrow -> keyPressed_Movement South
        | ConsoleKey.LeftArrow -> keyPressed_Movement West 
        | ConsoleKey.RightArrow -> keyPressed_Movement East
        | _ -> ()  

    member _.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 250

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> onKeyPressed k
               true
