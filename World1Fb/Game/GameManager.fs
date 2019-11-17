module GameManager
open agent_GameLog
open agent_Round
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open LoadAndSave
open SystemManager

type Game(wmrAll:EntityManager->unit, wmrEntity:EntityManager->uint32->unit, format:SaveGameFormats) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let entities = new EntityManager()
    let events = new EventManager(entities, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
 
    member _.Events = events
    member _.Entities = entities

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        agentForRound.Init sgd.Round
        entities.Init sgd.EntityHistory sgd.MaxEntityID sgd.Round
        sgd.Round

    member private me.saveGame =
        System.Threading.Thread.Sleep 100
        LoadAndSave.SaveGame 
            format
            { 
                EntityHistory = entities.GetAllHistory()
                MaxEntityID = entities.GetMaxID
                Round = agentForRound.Get() - 1u
                ScheduledEvents = events.GetSchedule
            }

    member private me.gameLoop (round:uint32) =
        let handleEndOfRound loops = 
            [|1..loops|] |> Array.iter (fun x -> 
                while (not systemMan.AllSystemsIdle || events.PendingUpdates) do 
                    if (round > 0u && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                    System.Threading.Thread.Sleep 1)
            gameLog.WriteLog
            entities.RecordHistory round
        events.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        handleEndOfRound 10
        // Uncomment for world-view: wmrAll entities; printfn "Round#%i" round
            
    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        let mutable _round = 0u

        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            _round <- me.loadGame filename
        | _ -> 
            initialForms 
            |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then events.RaiseEvent (CreateEntity { Components = cts }))
            System.Threading.Thread.Sleep 100

        // Uncomment for world-view: wmrAll entities; printfn "Round#%i" _round

        while  (ControllerSystem.GetInputForAllEntities entities gameLog _round wmrEntity) do
            me.gameLoop _round
            _round <- agentForRound.Increment
            
        me.saveGame // Exiting Game
    

