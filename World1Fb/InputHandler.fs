module InputHandler
open System

type InputHandler() =

    member _.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 250

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | _ -> //Raise Event
               true