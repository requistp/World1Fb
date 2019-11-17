module InputHandler
open ControllerComponent
open System


let AwaitKeyboardInput (controller:ControllerComponent) : ActionTypes * bool =
    let mutable _action = None

    let handleKeyPressed (k:ConsoleKeyInfo) = 
        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore

        match k.Key with 
        | ConsoleKey.Escape -> Some (Idle,false)
        | ConsoleKey.E -> if controller.ActionAllowed Eat then Some (Eat,true) else None
        | ConsoleKey.Spacebar -> Some (Idle,true)
        | ConsoleKey.M -> if controller.ActionAllowed Mate then Some (Mate,true) else None
        | ConsoleKey.RightArrow -> if controller.ActionAllowed Move_East then Some (Move_East,true) else None
        | ConsoleKey.UpArrow -> if controller.ActionAllowed Move_North then Some (Move_North,true) else None
        | ConsoleKey.DownArrow -> if controller.ActionAllowed Move_South then Some (Move_South,true) else None
        | ConsoleKey.LeftArrow -> if controller.ActionAllowed Move_West then Some (Move_West,true) else None
        | _ -> None

    while _action.IsNone do
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 2
        _action <- handleKeyPressed (Console.ReadKey(true))

    _action.Value

