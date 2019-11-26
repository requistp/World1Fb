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
open LocationTypes
open SystemManager

type Game(wmrWorld:EntityManager->unit, wmrEntity:EntityManager->EntityID->unit, renderType:RenderTypes, format:SaveGameFormats) =
    let agentForRound = new agent_Round()
    let gameLog = new agent_GameLog()
    let enm = new EntityManager()
    let evm = new EventManager(enm, gameLog, agentForRound.Get)
    let systemMan = new SystemManager()
 
    let waitForSystems round loops = 
        [|1..loops|] 
        |> Array.iter (fun x -> 
            while (not systemMan.AllSystemsIdle || evm.PendingUpdates || enm.PendingUpdates) do 
                if (round > RoundNumber(0u) && x > 1) then gameLog.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : %s" "xld" "End of round" "Cancelled pending more events" x "Events") 
                System.Threading.Thread.Sleep 1)

    member _.Events = evm
    member _.Entities = enm

    member private me.loadGame filename =
        let sgd = LoadAndSave.LoadGame format filename 
        agentForRound.Init sgd.Round
        enm.Init sgd.Components sgd.ComponentTypes sgd.Entities sgd.Locations
        sgd.Round

    member private me.saveGame =
        System.Threading.Thread.Sleep 100
        let sgd = 
            { 
                Components = enm.GetForSave_Components
                ComponentTypes = enm.GetForSave_ComponentTypes
                Entities = enm.GetForSave_Entities
                Locations = enm.GetForSave_Locations
                Round = agentForRound.Get() - 1u
                ScheduledEvents = evm.GetSchedule
            }
        LoadAndSave.SaveGame format sgd (agentForRound.Get())

    member private me.gameLoop (round:RoundNumber) =
        evm.ExecuteScheduledEvents round
        systemMan.UpdateSystems round
        waitForSystems round 5
        gameLog.WriteLog

        if (renderType = World) then 
            Async.Parallel 
            (
                wmrWorld enm
            )
        System.Console.SetCursorPosition(MapWidth,25)
        printfn "Round:%i" round.ToUint32

    member me.Start (ss:AbstractSystem[]) (initialForms:Component[][]) (filename:string) = 
        let mutable _round = RoundNumber(0u)

        systemMan.Init ss

        match initialForms.Length with
        | 0 -> 
            _round <- me.loadGame filename
        | _ -> 
            initialForms 
            |> Array.Parallel.iter (fun cts -> if (cts.Length > 0) then evm.RaiseEvent (CreateEntity cts))
            waitForSystems (RoundNumber(0u)) 5
            VisionSystem.UpdateViewableForAll enm (RoundNumber(0u))
        
        if (renderType = World) then 
            Async.Parallel 
            (
                wmrWorld enm
            )

        while  (ControllerSystem.GetInputForAllEntities enm gameLog _round (if renderType=Entity then Some wmrEntity else None)) && (_round.ToUint32<500u) do
            me.gameLoop _round
            _round <- agentForRound.Increment
            VisionSystem.UpdateViewableForAll enm _round

        me.saveGame // Exiting Game
    

