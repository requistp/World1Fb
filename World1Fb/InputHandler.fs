module InputHandler
open GameEvents
open EventManager
open MovementComponent
open System


type InputHandler(em:EventManager) =

    let OnKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> em.QueueEvent(GameEventData_Movement_KeyPressed(North))
        | ConsoleKey.DownArrow -> em.QueueEvent(GameEventData_Movement_KeyPressed(South))
        | ConsoleKey.LeftArrow -> em.QueueEvent(GameEventData_Movement_KeyPressed(West))
        | ConsoleKey.RightArrow -> em.QueueEvent(GameEventData_Movement_KeyPressed(East))
        | _ -> ()  

    member _.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 250

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> OnKeyPressed k
               true
