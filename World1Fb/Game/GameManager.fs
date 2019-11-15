module GameManager
open Entities
open agent_GameLog
open agent_Round
open Component
open ComponentEnums
open ControllerSystem
open EntityManager
open EventManager
open EventTypes
open InputHandler
open LoadAndSave
open LocationTypes
open System
open SystemManager


type Game(wmr:Entities->uint32->unit, format:SaveGameFormats) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let entities = new Entities()
    let eventMan = new EventManager(entities, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
    let inputMan = new InputHandler(eventMan, entities)
 
    let setInitialForms (initialForms:Component[][]) = 
        initialForms 
        |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then eventMan.RaiseEvent (CreateEntity { Components = cts }))

    member _.EventManager = eventMan
    member _.EntityManager = entities
    member _.Logger = gameLog
    member _.GetRound() = agentForRound.Get()

    member private me.assignController =
        match entities.GetEntitiesWithComponent ControllerComponentID with
        | [||] -> None
        | l -> Some l.[0]

    member private me.gameLoop =
        let waitForEndOfRound round = 
            let waitLoop loops =
                let checkIdle x = 
                    while (not systemMan.AllSystemsIdle || eventMan.PendingUpdates) do 
                        if (round > 0u && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                        System.Threading.Thread.Sleep 2
                [|1..loops|] |> Array.iter (fun x -> checkIdle x)
            waitLoop 20

        let round = agentForRound.Get()
        eventMan.ExecuteScheduledEvents round
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
        LoadAndSave.SaveGame 
            format
            { 
                Round = agentForRound.Get() // Maybe this should be -1u
                ECMap = entities.GetEntityMap()
                MaxEntityID = entities.GetMaxID
                ScheduledEvents = eventMan.GetSchedule
            }

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            me.loadGame filename
            me.assignController |> inputMan.SetEntityID
            wmr entities (inputMan.GetEntityID.Value)
            //printfn "Round#%i      " (eventMan.GetRound())
        | _ -> 
            setInitialForms initialForms
            System.Threading.Thread.Sleep 250
            me.assignController |> inputMan.SetEntityID
            me.gameLoop
        
        let mutable r = inputMan.AwaitKeyboardInput
        while r <> ExitGame do
            if r = GameAction then me.gameLoop
            r <- inputMan.AwaitKeyboardInput

        System.Threading.Thread.Sleep 250

        me.saveGame
    



