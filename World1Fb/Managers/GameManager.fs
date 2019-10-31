module GameManager
open Component
open AbstractSystem
open CommonGenericFunctions
open EntityManager
open EventManager
open EventTypes
open InputHandler
open SystemManager
open System


type Game(renderer:EntityManager->uint32->unit, renderer_SetContent:(string*string)[]->bool->Async<unit>, renderer_SetDisplay:string->unit, renderer_Display:string->unit, wmr:EntityManager->unit, wmrKeys:ConsoleKey->unit) =
    let mutable _round = 0u // I do this because when I wasn't getting the round there were problems
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager(entityMan, eventMan)
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    member this.EventManager = eventMan
    member this.EntityManager = entityMan
    member this.SystemManager = systemMan
    member this.Round() = _round

    member private this.assignController =
        match entityMan.GetEntitiesWithComponent ControllerData.ID with
        | [||] -> None
        | l -> Some l.[0]

    member private this.setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> 
            if (cts.Length > 0) then 
                eventMan.QueueEvent (EventData_CreateEntity cts)
            )

    member this.Start (ss:AbstractSystem[]) (initialForms:Component[][]) = 
        systemMan.Initialize ss
        this.setInitialForms initialForms
        
        this.gameLoop

        this.assignController |> inputMan.SetEntityID

        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then this.gameLoop
            r <- inputMan.AwaitKeyboardInput

    member private this.gameLoop =
        systemMan.UpdateSystems

        _round <- eventMan.EndRound
        printfn "%i    " _round // EndRound seems to hang if I don't print this.

        wmr entityMan
        

