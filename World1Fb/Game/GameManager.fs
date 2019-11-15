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
open SystemManager


type Game(wmr:EntityManager->uint32->unit, format:SaveGameFormats) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let entities = new EntityManager()
    let events = new EventManager(entities, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(events,entities)
 
    member _.Events = events
    member _.Entities = entities

    member private me.assignController = // Does not work as let statement
        match entities.GetEntitiesWithComponent ControllerComponentID with
        | [||] -> None
        | l -> Some l.[0]
    member private me.gameLoop =
        let waitForEndOfRound round = 
            let waitLoop loops =
                let checkIdle x = 
                    while (not systemMan.AllSystemsIdle || events.PendingUpdates) do 
                        if (round > 0u && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                        System.Threading.Thread.Sleep 2
                [|1..loops|] |> Array.iter (fun x -> checkIdle x)
            waitLoop 20

        let round = agentForRound.Get()
        events.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        waitForEndOfRound round
        gameLog.WriteLog
        entities.RecordHistory round (entities.GetEntityMap(),entities.GetComponentMap(),entities.GetLocationMap())
        ControllerSystem.UpdateCurrentActionsForAllEntities entities gameLog round
        wmr entities (inputMan.GetEntityID.Value); printfn "Round#%i" round       
        agentForRound.Increment
    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        agentForRound.Init sgd.Round
        entities.Init sgd.ECMap sgd.MaxEntityID
    member private me.saveGame =
        System.Threading.Thread.Sleep 250
        LoadAndSave.SaveGame 
            format
            { 
                Round = agentForRound.Get() // Maybe this should be -1u
                ECMap = entities.GetEntityMap()
                MaxEntityID = entities.GetMaxID
                ScheduledEvents = events.GetSchedule
            }

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            me.loadGame filename
            me.assignController |> inputMan.SetEntityID
            wmr entities (inputMan.GetEntityID.Value)
        | _ -> 
            initialForms |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then events.RaiseEvent (CreateEntity { Components = cts }))
            me.assignController |> inputMan.SetEntityID
            me.gameLoop
        
        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

        me.saveGame // Exiting Game
    
