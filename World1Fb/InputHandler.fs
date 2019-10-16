module InputHandler
open AbstractComponent
open EntityManager
open EventManager
open GameEvents
open MovementComponent
open System


type InputHandler(evm:EventManager, enm:EntityManager) =
    let mutable _entityID = None

    member private this.HasRequiredComponents (cts:ComponentTypes[]) =
        match _entityID with
        | None -> None
        | Some eid -> match enm.EntityHasAllComponents cts eid with
                      | false -> None
                      | true -> Some eid

    //member private this.keyPressed_Eat =
    //    match this.HasRequiredComponents [|ComponentTypes.Eating|] with
    //    | None -> ()
    //    | Some eid -> evm.QueueEvent(Event_KeyPressed_Eat(eid))

    member private this.keyPressed_Movement d = 
        match this.HasRequiredComponents [| ComponentTypes.Form; ComponentTypes.Movement |] with
        | None -> ()
        | Some eid -> evm.QueueEvent (Event_Action_Movement(eid,d))

    member private this.onKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> this.keyPressed_Movement North 
        | ConsoleKey.DownArrow -> this.keyPressed_Movement South
        | ConsoleKey.LeftArrow -> this.keyPressed_Movement West 
        | ConsoleKey.RightArrow -> this.keyPressed_Movement East
        //| ConsoleKey.E -> this.keyPressed_Eat
        | _ -> ()  

        while Console.KeyAvailable do //Might helpclear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
            
    member this.SetEntityID eid = _entityID <- eid

    member this.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 1

        match Console.ReadKey(true).Key with
        | ConsoleKey.Escape -> false
        | k -> this.onKeyPressed k
               true
