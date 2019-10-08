module InputHandler
open System
open EventManager
open MovementComponent

type InputHandler(em:EventManager) =

    let OnKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> em.QueueEvent(GameEvent.KeyPressed_Movement North)
        | ConsoleKey.DownArrow -> em.QueueEvent(GameEvent.KeyPressed_Movement South)
        | ConsoleKey.LeftArrow -> em.QueueEvent(GameEvent.KeyPressed_Movement West)
        | ConsoleKey.RightArrow -> em.QueueEvent(GameEvent.KeyPressed_Movement East)
        | _ -> ()  

    member _.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 250

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> OnKeyPressed k
               true
