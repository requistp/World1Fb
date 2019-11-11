module GameManager
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open InputHandler
open LoadAndSave
open LocationTypes
open MemoryManager
open System
open SystemManager

type Game(renderer_SetDisplay:string->unit, wmr:EntityManager->uint32->unit, wmrKeys:ConsoleKey->unit, format:SaveGameFormats) =
    let entityMan = new EntityManager()
    let eventMan = new EventManager(entityMan)
    let memMan = new MemoryManager()
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    let setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then eventMan.RaiseEvent (CreateEntity { Components = cts }))

    member _.EventManager = eventMan
    member _.EntityManager = entityMan
    member _.MemoryManager = memMan

    member private me.assignController =
        match ControllerComponentID|>entityMan.GetEntitiesWithComponent with
        | [||] -> None
        | l -> Some l.[0]

    member private me.gameLoop =
        let round = eventMan.GetRound()
        eventMan.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        eventMan.EndRound round
        wmr entityMan (inputMan.GetEntityID.Value)
        printfn "Round#%i      " round

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        eventMan.InitRound sgd.Round
        entityMan.Init sgd.ECMap
        entityMan.Init sgd.MaxEntityID

    member private me.saveGame =
        LoadAndSave.SaveGame 
            format
            { 
                Round = eventMan.GetRound() // Maybe this should be -1u
                ECMap = entityMan.GetEntities()
                MaxEntityID = entityMan.GetMaxID
                ScheduledEvents = eventMan.GetSchedule
            }

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            me.loadGame filename
            me.assignController |> inputMan.SetEntityID
            wmr entityMan (inputMan.GetEntityID.Value)
            //printfn "Round#%i      " (eventMan.GetRound())
        | _ -> 
            setInitialForms initialForms
            System.Threading.Thread.Sleep 50
            me.assignController |> inputMan.SetEntityID
            me.gameLoop
        
        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

        System.Threading.Thread.Sleep 50

        me.saveGame
    



