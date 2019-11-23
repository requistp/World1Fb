module GameManager
open agent_GameLog
open agent_Round
open CommonGenericFunctions
open Component
open ComponentEnums
open EntityManager
open EventManager
open EventTypes
open LoadAndSave
open SystemManager

type Game(wmrAll:EntityManager->RoundNumber option->unit, wmrEntity:EntityManager->EntityID->unit, format:SaveGameFormats, useHistory:bool) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let entities = new EntityManager(useHistory)
    let events = new EventManager(entities, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
 
    member _.Events = events
    member _.Entities = entities

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        agentForRound.Init sgd.Round
        entities.Init sgd.Entities
        sgd.Round

    member private me.saveGame =
        System.Threading.Thread.Sleep 100
        LoadAndSave.SaveGame 
            format
            { 
                Entities = entities.GetForSave
                Round = agentForRound.Get() - 1u
                ScheduledEvents = events.GetSchedule
            }

    member private me.gameLoop (round:RoundNumber) =
        let handleEndOfRound loops = 
            [|1..loops|] |> Array.iter (fun x -> 
                while (not systemMan.AllSystemsIdle || events.PendingUpdates) do 
                    if (round > RoundNumber(0u) && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                    System.Threading.Thread.Sleep 1)
            gameLog.WriteLog

        events.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        handleEndOfRound 5

        // Uncomment for world-view: 
        wmrAll entities None; printfn "Round#%i" round.ToUint32
            
    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        let mutable _round = RoundNumber(0u)

        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            _round <- me.loadGame filename
        | _ -> 
            initialForms 
            |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then events.RaiseEvent (CreateEntity cts))
            System.Threading.Thread.Sleep 1000
        
        // Uncomment for world-view: 
        wmrAll entities None; printfn "Round#%i" _round.ToUint32

        while  (ControllerSystem.GetInputForAllEntities entities gameLog _round wmrEntity) && (_round.ToUint32<500u) do
            me.gameLoop _round
            _round <- agentForRound.Increment
            
        me.saveGame // Exiting Game
    

