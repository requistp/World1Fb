module EventManager
open agent_EventListeners
open agent_EventSchedule
open agent_GameEventLog
open agent_Round
open EntityManager
open EventTypes
open LocationTypes
open System


type EventManager(enm:EntityManager) =
    let agentForRound = new agent_Round()
    let agentForLog = new agent_GameEventLog()
    let agentForListeners = new agent_EventListeners(agentForLog)
    let agentForSchedule = new agent_EventSchedule(agentForLog, agentForListeners, enm)

    member _.EndRound round =
        System.Console.SetCursorPosition(0,MapHeight+1)
        Console.Write "   "
        System.Threading.Thread.Sleep 50
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates ) do 
            agentForLog.Log_EndOfRoundCancelled round 1 "Events"
            System.Console.SetCursorPosition(0,MapHeight+1)
            Console.Write "1 "
            System.Threading.Thread.Sleep 3
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates ) do 
            agentForLog.Log_EndOfRoundCancelled round 2 "Events"
            System.Console.SetCursorPosition(0,MapHeight+1)
            Console.Write "2 "
            System.Threading.Thread.Sleep 3
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates ) do 
            agentForLog.Log_EndOfRoundCancelled round 3 "Events"
            System.Console.SetCursorPosition(0,MapHeight+1)
            Console.Write "3 "
            System.Threading.Thread.Sleep 3
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates ) do
            agentForLog.Log_EndOfRoundCancelled round 4 "Events"
            System.Console.SetCursorPosition(0,MapHeight+1)
            Console.Write "4 "
            System.Threading.Thread.Sleep 3
        while (agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates ) do 
            agentForLog.Log_EndOfRoundCancelled round 5 "Events"
            System.Console.SetCursorPosition(0,MapHeight+1)
            Console.Write "5 "
            System.Threading.Thread.Sleep 3
        agentForLog.WriteLog
        System.Console.SetCursorPosition(0,MapHeight+2)
        Console.Write "                          "
        while enm.PendingUpdates do
            agentForLog.Log_EndOfRoundCancelled round 1 "Entities"
            System.Console.SetCursorPosition(0,MapHeight+2)
            Console.Write "entity events: 1 "
            System.Threading.Thread.Sleep 3
        while enm.PendingUpdates do
            agentForLog.Log_EndOfRoundCancelled round 2 "Entities"
            System.Console.SetCursorPosition(0,MapHeight+2)
            Console.Write "entity events: 2 "
            System.Threading.Thread.Sleep 3
        while enm.PendingUpdates do
            agentForLog.Log_EndOfRoundCancelled round 3 "Entities"
            System.Console.SetCursorPosition(0,MapHeight+2)
            Console.Write "entity events: 3 "
            System.Threading.Thread.Sleep 3
        while enm.PendingUpdates do
            agentForLog.Log_EndOfRoundCancelled round 4 "Entities"
            System.Console.SetCursorPosition(0,MapHeight+2)
            Console.Write "entity events: 4 "
            System.Threading.Thread.Sleep 3
        while enm.PendingUpdates do
            agentForLog.Log_EndOfRoundCancelled round 5 "Entities"
            System.Console.SetCursorPosition(0,MapHeight+2)
            Console.Write "entity events: 5 "
            System.Threading.Thread.Sleep 3
        enm.RecordHistory round
        agentForRound.Increment

    member _.RaiseEvent (ge:GameEventTypes) = agentForListeners.Execute (agentForRound.Get()) ge

    member _.ExecuteScheduledEvents round = agentForSchedule.ExecuteScheduled round

    member _.GetRound() = agentForRound.Get()

    member _.GetSchedule = agentForSchedule.Get

    member _.InitRound round = agentForRound.Init round 

    member _.RegisterListener (listener:string) (eventTypeID:byte) (callback:GameEventCallback) = agentForListeners.Register (agentForRound.Get()) listener eventTypeID callback
    
    member _.ScheduleEvent (se:GameEventTypes) = agentForSchedule.Schedule (agentForRound.Get()) se
        
    member _.SetLogging (toggle:bool) = agentForLog.SetLogging toggle

