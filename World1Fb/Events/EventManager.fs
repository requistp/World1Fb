module EventManager
open agent_EventListeners
open agent_EventSchedule
open agent_GameEventLog
open agent_Round
open EntityManager
open EventTypes
open System


type EventManager(enm:EntityManager) =
    let agentForRound = new agent_Round()
    let agentForLog = new agent_GameEventLog()
    let agentForListeners = new agent_EventListeners(agentForLog)
    let agentForSchedule = new agent_EventSchedule(agentForLog, agentForListeners, enm)

    member _.EndRound =
        let round = agentForRound.Get
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            //agentForLog.Log_EndOfRoundCancelled round 1
            Console.Write "1"
            System.Threading.Thread.Sleep 7
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 2
            Console.Write "2"
            System.Threading.Thread.Sleep 6
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 3
            Console.Write "3"
            System.Threading.Thread.Sleep 5
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 4
            Console.Write "4"
            System.Threading.Thread.Sleep 4
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 5
            Console.Write "5"
            System.Threading.Thread.Sleep 3
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 6
            Console.Write "6"
            System.Threading.Thread.Sleep 2
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates || enm.PendingUpdates) do
            agentForLog.Log_EndOfRoundCancelled round 7
            Console.Write "7"
            System.Threading.Thread.Sleep 1
        agentForLog.WriteLog
        agentForRound.Increment

    member _.ExecuteEvent (ge:GameEventTypes) = 
        agentForListeners.Execute agentForRound.Get ge

    member _.ExecuteScheduledEvents = 
        agentForSchedule.ExecuteScheduled agentForRound.Get

    member _.GetRound() =
        agentForRound.Get

    member _.RegisterListener (listener:string) (eventTypeID:byte) (callback:GameEventCallback) = 
        agentForListeners.Register agentForRound.Get listener eventTypeID callback
    
    member _.ScheduleEvent (se:GameEventTypes) = 
        agentForSchedule.Schedule agentForRound.Get se
        
    member _.SetLogging (toggle:bool) = 
        agentForLog.SetLogging toggle

