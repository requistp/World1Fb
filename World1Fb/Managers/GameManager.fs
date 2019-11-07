module GameManager
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open InputHandler
open LoadAndSave
open LocationTypes
open System
open SystemManager

type Game(renderer_SetDisplay:string->unit, wmr:EntityManager->unit, wmrKeys:ConsoleKey->unit, format:SaveGameFormats) =
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    member _.EventManager = eventMan
    member _.EntityManager = entityMan

    member private me.assignController =
        match entityMan.GetEntitiesWithComponent ControllerComponentID with
        | [||] -> None
        | l -> Some l.[0]

    member private me.setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then eventMan.ExecuteEvent (CreateEntity { Components = cts }))

    member private me.gameLoop =
        //System.Console.SetCursorPosition(0,MapHeight+2)
        //Console.Write "                          "
        //while entityMan.PendingUpdates do
        //    System.Console.SetCursorPosition(0,MapHeight+2)
        //    Console.Write "entity events: 1 "
        //    System.Threading.Thread.Sleep 5
        //while entityMan.PendingUpdates do
        //    System.Console.SetCursorPosition(0,MapHeight+2)
        //    Console.Write "entity events: 2 "
        //    System.Threading.Thread.Sleep 5
        eventMan.ExecuteScheduledEvents
        systemMan.UpdateSystems
        eventMan.EndRound
        wmr entityMan
        printfn "Round#%i      " (eventMan.GetRound())

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        eventMan.InitRound sgd.Round
        entityMan.Init sgd.ECMap
        entityMan.Init sgd.MaxEntityID

    member private me.saveGame =
        LoadAndSave.SaveGame 
            format
            { 
                Round = eventMan.GetRound() 
                ECMap = entityMan.GetMap
                MaxEntityID = entityMan.GetMaxID
                ScheduledEvents = eventMan.GetSchedule
            }

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            me.loadGame filename
            wmr entityMan
            printfn "Round#%i      " (eventMan.GetRound())
        | _ -> 
            me.setInitialForms initialForms
            me.gameLoop
        
        me.assignController |> inputMan.SetEntityID

        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

        me.saveGame
    
    //member me.RunGame = 
    //    systemMan.Init ss
    //    me.setInitialForms initialForms
    //    me.assignController |> inputMan.SetEntityID
        
    //    me.gameLoop
        
    //    let mutable r = inputMan.AwaitKeyboardInput
    //    while r <> ExitGame do
    //        if r = GameAction then me.gameLoop
    //        r <- inputMan.AwaitKeyboardInput

    //    me.SaveGame
        
    
    
    
