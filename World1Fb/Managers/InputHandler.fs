module InputHandler
open AbstractComponent
open EatingComponent
open EntityManager
open EventManager
open FoodComponent
open FormComponent
open FrameManager
open EventTypes
open LocationTypes
open MovementComponent
open System
open SystemManager

type KeyboardResult = 
    | ExitGame
    | GameAction
    | InfoOnly

type InputHandler(evm:EventManager, enm:EntityManager, fman:FrameManager, sysm:SystemManager) =
    let mutable _entityID = None

    member this.EntityID = _entityID

    member private this.HaveEntityAndRequiredComponents (cts:'T[]) =
        match _entityID with
        | None -> false
        | Some eid -> enm.EntityHasAllComponents cts eid
    
    member private this.HandleAction (requiredCTS:'T[]) event =
        match this.HaveEntityAndRequiredComponents requiredCTS with
        | false -> ()
        | true -> event

    member private this.onKeyPressed k = 
        match k with 
        | ConsoleKey.UpArrow -> this.HandleAction [|typeof<FormComponent>; typeof<MovementComponent>|] (evm.QueueEvent (EventData_Action_Movement(_entityID.Value,North)))
        | ConsoleKey.DownArrow -> this.HandleAction [|typeof<FormComponent>; typeof<MovementComponent>|] (evm.QueueEvent (EventData_Action_Movement(_entityID.Value,South)))
        | ConsoleKey.LeftArrow -> this.HandleAction [|typeof<FormComponent>; typeof<MovementComponent>|] (evm.QueueEvent (EventData_Action_Movement(_entityID.Value,West)))
        | ConsoleKey.RightArrow -> this.HandleAction [|typeof<FormComponent>; typeof<MovementComponent>|] (evm.QueueEvent (EventData_Action_Movement(_entityID.Value,East)))
        | ConsoleKey.E -> this.HandleAction [|typeof<EatingComponent>|] (evm.QueueEvent(EventData_Generic(Action_Eat,_entityID.Value)))
        | _ -> ()  

        while Console.KeyAvailable do //Might help clear double movement keys entered in one turn
            Console.ReadKey(true).Key |> ignore
        GameAction
        
    member this.SetEntityID eid = _entityID <- eid

    member this.AwaitKeyboardInput =
        while not Console.KeyAvailable do
            System.Threading.Thread.Sleep 1

        let k = Console.ReadKey(true)
        match k.Key with
        | ConsoleKey.Escape -> ExitGame
        | ConsoleKey.F12 -> this.DisplayGameEvents (k.Modifiers = ConsoleModifiers.Shift) (k.Modifiers = ConsoleModifiers.Control)
        | k -> this.onKeyPressed k
               
    member private this.DisplayGameEvents (shift:bool) (ctrl:bool) =
        let printRes (res:Result<string option,string>) =
            match res with
            | Ok o -> "Ok "
            | Error s -> "Err"
        let printResString (res:Result<string option,string>) =
            match res with
            | Error s -> sprintf "/ %s" s
            | Ok o -> match o with
                      | None -> ""
                      | Some s -> sprintf "/ %s" s
        let printGER (n:uint32) ((age,res):GameEventResult) = 
            printfn "%i|%s: %s %s" n (printRes res) age.ToString (printResString res)
        let lt = 
            match (shift,ctrl) with
            | true,_ -> All
            | _,true -> AllExceptFirst
            | _ -> Current
        
        fman.GameEventsAll lt 
        |> Array.iter (fun (n,gers) -> gers |> Array.iter (fun ger -> printGER n ger))
        //|> Array.iter (fun (n,ger) -> printGER ger)
        InfoOnly


