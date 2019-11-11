module EventManager
open agent_EventListeners
open agent_EventSchedule
open agent_GameLog
open EntityManager
open EventTypes


type EventManager(enm:EntityManager, log:agent_GameLog, getRound:unit->uint32) =
    let agentForListeners = new agent_EventListeners(log)
    let agentForSchedule = new agent_EventSchedule(log, agentForListeners, enm)

    member _.ExecuteScheduledEvents round = agentForSchedule.ExecuteScheduled round

    member _.GetSchedule = agentForSchedule.Get

    member _.PendingUpdates = agentForListeners.PendingUpdates || agentForSchedule.PendingUpdates

    member _.RaiseEvent (ge:GameEventTypes) = agentForListeners.Execute (getRound()) ge

    member _.RegisterListener (listener:string) (eventTypeID:byte) (callback:GameEventCallback) = agentForListeners.Register listener eventTypeID callback
    
    member _.ScheduleEvent (se:GameEventTypes) = agentForSchedule.Schedule (getRound()) se
        


