﻿module agent_EventSchedule
open agent_GameEventLog
open agent_EventListeners
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes


type private agent_ScheduleMsg =
| ExecuteScheduled of round:uint32 
| Init of Map<uint32,GameEventTypes[]>
| Get  of AsyncReplyChannel<Map<uint32,GameEventTypes[]> >
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
                                match isNew && sed.Schedule = RepeatIndefinitely with
                                | true -> uint32 (TimingOffset (int sed.Frequency))
                                | false -> sed.Frequency                                
                            _schedule <- Map_AppendValueToArrayNonUnique _schedule (round+interval) se
                            agentForLog.Log_ScheduledEvent round se
                        match msg with
                        | ExecuteScheduled round ->
                            let reschedule (se:GameEventTypes) =
                                let sed,ge = se.ToScheduleEvent
                                match sed.Schedule with
                                | RunOnce -> ()
                                | RepeatIndefinitely -> addToSchedule round se false 
                                | RepeatFinite x -> 
                                    match x with
                                    | 1u -> () // Done, that 1 is the last one
                                    | _ -> addToSchedule round (ScheduleEvent ({ sed with Schedule = RepeatFinite (x - 1u) }, ge)) false                                 
                            let executeAndReschedule (se:GameEventTypes) =
                                let _,ge = se.ToScheduleEvent
                                match enm.Exists ge.EntityID with
                                | false -> ()
                                | true ->
                                    agentForListeners.Execute round ge
                                    reschedule se
                            match _schedule.ContainsKey(round) with
                            | false -> ()
                            | true -> 
                                _schedule.Item(round) |> Array.Parallel.iter (fun se -> executeAndReschedule se)
                                _schedule <- _schedule.Remove(round)
                        | Get replyChannel ->
                            replyChannel.Reply(_schedule)
                        | Init map ->
                            _schedule <- map
                        | Schedule (round,se) -> 
                            addToSchedule round se true
                }
            )

    member _.ExecuteScheduled round = agent.Post (ExecuteScheduled round)

    member _.Init map = agent.Post (Init map)

    member _.Get = agent.PostAndReply Get

    member _.PendingUpdates = agent.CurrentQueueLength > 0

    member _.Schedule round se = agent.Post (Schedule (round,se))

