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
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager(entityMan, eventMan)
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    member _.EventManager = eventMan
    member _.EntityManager = entityMan
    member _.SystemManager = systemMan

    member private me.assignController =
        match entityMan.GetEntitiesWithComponent ControllerComponent.ID with
        | [||] -> None
        | l -> Some l.[0]

    member private me.setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> 
            if (cts.Length > 0) then 
                eventMan.ExecuteEvent (CreateEntity { EntityID=cts.[0].EntityID; Components=cts })
            )

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) = 
        systemMan.Initialize ss
        me.setInitialForms initialForms
        
        me.gameLoop

        me.assignController |> inputMan.SetEntityID

        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

    member private me.gameLoop =
        eventMan.ExecuteScheduledEvents
        
        systemMan.UpdateSystems

        eventMan.EndRound

        printfn "%i    " (eventMan.GetRound())

        wmr entityMan
        

