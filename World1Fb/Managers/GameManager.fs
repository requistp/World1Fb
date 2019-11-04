module GameManager
open Component
open EntityManager
open EventManager
open EventTypes
open InputHandler
open System
open SystemManager


type Game(renderer_SetDisplay:string->unit, wmr:EntityManager->unit, wmrKeys:ConsoleKey->unit) =
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    member _.EventManager = eventMan
    member _.EntityManager = entityMan

    member private me.assignController =
        match entityMan.GetEntitiesWithComponent ControllerComponent.ID with
        | [||] -> None
        | l -> Some l.[0]

    member private me.setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then eventMan.ExecuteEvent (CreateEntity { EntityID=cts.[0].EntityID; Components=cts }))

    member private me.gameLoop =
        eventMan.ExecuteScheduledEvents
        systemMan.UpdateSystems
        eventMan.EndRound
        wmr entityMan
        printfn "Round#%i      " (eventMan.GetRound())

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) = 
        systemMan.Initialize ss
        me.setInitialForms initialForms
        me.assignController |> inputMan.SetEntityID
        
        me.gameLoop
        
        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

        

