module GameManager
open agent_GameLog
open agent_Round
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


type Game(renderer_SetDisplay:string->unit, wmr:EntityManager->uint32->unit, wmrKeys:ConsoleKey->unit, format:SaveGameFormats) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let entityMan = new EntityManager(gameLog)
    let eventMan = new EventManager(entityMan, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(eventMan, entityMan, renderer_SetDisplay, wmrKeys)
 
    let setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then eventMan.RaiseEvent (CreateEntity { Components = cts }))

    member _.EventManager = eventMan
    member _.EntityManager = entityMan
    member _.Logger = gameLog
    member _.GetRound() = agentForRound.Get()

    member private me.assignController =
        match ControllerComponentID|>entityMan.GetEntitiesWithComponent with
        | [||] -> None
        | l -> Some l.[0]

    member private me.gameLoop =
        let endOfRoundTasks round = 
            let waitForEndOfRound round loops =
                let checkIdle x = 
                    while (not systemMan.AllSystemsIdle || eventMan.PendingUpdates) do 
                        if (round > 0u && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                        System.Threading.Thread.Sleep 3
                [|1..loops|] |> Array.iter (fun x -> checkIdle x)
            waitForEndOfRound round 20
            gameLog.WriteLog
            entityMan.RecordHistory round
        let round = agentForRound.Get()

        eventMan.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        endOfRoundTasks round
        wmr entityMan (inputMan.GetEntityID.Value); printfn "Round#%i" round       
        agentForRound.Increment

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        agentForRound.Init sgd.Round
        entityMan.Init sgd.ECMap
        entityMan.Init sgd.MaxEntityID

    member private me.saveGame =
        LoadAndSave.SaveGame 
            format
            { 
                Round = agentForRound.Get() // Maybe this should be -1u
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
    



