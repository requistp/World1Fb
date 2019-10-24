module InputHandler
open AbstractComponent
open EatingComponent
open EntityManager
open EventManager
open FormComponent
open FrameManager
open EventTypes
open MovementComponent
open System
open SystemManager

type KeyboardResult = 
    | ExitGame
    | GameAction
    | InfoOnly

type InputHandler(evm:EventManager, enm:EntityManager, fman:FrameManager, sysm:SystemManager, renderer_SetDisplay:string->unit) =
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

    member this.SetDisplay k =
        match k with
        | ConsoleKey.F1 -> renderer_SetDisplay "World Map"
        | ConsoleKey.F2 -> renderer_SetDisplay "Game Events List"
        | _ -> ()
        InfoOnly
        
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
        | ConsoleKey.F1 -> this.SetDisplay k.Key
        | ConsoleKey.F2 -> this.SetDisplay k.Key
        | k -> this.onKeyPressed k
               
