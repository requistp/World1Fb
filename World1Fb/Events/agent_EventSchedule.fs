module agent_EventSchedule
open agent_GameEventLog
open agent_EventListeners
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes


type private agent_ScheduleMsg =
    | ExecuteScheduled of round:uint32 
    | Schedule of round:uint32 * GameEventTypes


type agent_EventSchedule(agentForLog:agent_GameEventLog, agentForListeners:agent_EventListeners, enm:EntityManager) =

    let agent =
        let mutable _schedule = Map.empty<uint32,GameEventTypes[]>
        MailboxProcessor<agent_ScheduleMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let addToSchedule round (se:GameEventTypes) isNew =
                            let sed,_ = se.ToScheduleEvent
                            let interval = 
                                match isNew with
                                | false -> sed.Frequency
                                | true -> uint32 (TimingOffset (int sed.Frequency))
                            _schedule <- Map_AppendValueToArrayNonUnique _schedule (round+interval) se
                            agentForLog.Log_ScheduledEvent round se
                        match msg with
                        | ExecuteScheduled round ->
                            let executeAndReschedule (se:GameEventTypes) =
                                let _,ge = se.ToScheduleEvent
                                match enm.Exists ge.EntityID with
                                | false -> ()
                                | true ->
                                    agentForListeners.Execute round ge
                                    addToSchedule round se false 
                            match _schedule.ContainsKey(round) with
                            | false -> ()
                            | true -> 
                                _schedule.Item(round) |> Array.Parallel.iter (fun se -> executeAndReschedule se)
                                _schedule <- _schedule.Remove(round)
                        | Schedule (round,se) -> 
                            addToSchedule round se true
                }
            )

    member _.ExecuteScheduled round =
        agent.Post (ExecuteScheduled round)

    member _.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member _.Schedule round se =
        agent.Post (Schedule (round,se))


