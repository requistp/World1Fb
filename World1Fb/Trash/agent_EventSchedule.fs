module agent_EventSchedule
//open agent_GameLog
//open agent_EventListeners
//open CalendarTimings
//open CommonGenericFunctions
//open agent_Entities
//open EventTypes


//type private agent_ScheduleMsg =
//    | AddToSchedule of round:uint32 * GameEventTypes
//    | ExecuteScheduled of round:uint32 
//    | Init of Map<uint32,GameEventTypes[]>
//    | Get  of AsyncReplyChannel<Map<uint32,GameEventTypes[]> >


//type agent_EventSchedule(log:agent_GameLog, agentForListeners:agent_EventListeners, enm:agent_Entities) =

//    let agent =
//        let mutable _schedule = Map.empty<uint32,GameEventTypes[]>
//        MailboxProcessor<agent_ScheduleMsg>.Start(
//            fun inbox ->
//                async { 
//                    while true do
//                        let! msg = inbox.Receive()
//                        let addToSchedule round (se:GameEventTypes) isNew =
//                            let sed,ge = se.ToScheduleEvent
//                            let interval = 
//                                match isNew && sed.Schedule = RepeatIndefinitely with
//                                | true -> uint32 (TimingOffset (int sed.Frequency))
//                                | false -> sed.Frequency                                
//                            _schedule <- Map_AppendValueToArrayNonUnique _schedule (round+interval) se
//                            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : Frequency:%i" "-->" "Scheduled Event" (ge.GameEventType()) ge.EntityID sed.Frequency)
//                        match msg with
//                        | AddToSchedule (round,se) -> 
//                            addToSchedule round se true
//                        | ExecuteScheduled round ->
//                            let reschedule (se:GameEventTypes) =
//                                let sed,ge = se.ToScheduleEvent
//                                match sed.Schedule with
//                                | RunOnce -> ()
//                                | RepeatIndefinitely -> addToSchedule round se false 
//                                | RepeatFinite x -> 
//                                    match x with
//                                    | 1u -> () // Done, that 1 is the last one
//                                    | _ -> addToSchedule round (ScheduleEvent ({ sed with Schedule = RepeatFinite (x - 1u) }, ge)) false                                 
//                            let executeAndReschedule (se:GameEventTypes) =
//                                let _,ge = se.ToScheduleEvent
//                                match enm.EntityExists ge.EntityID with
//                                | false -> ()
//                                | true ->
//                                    agentForListeners.Execute round ge
//                                    reschedule se
//                            match _schedule.ContainsKey(round) with
//                            | false -> ()
//                            | true -> 
//                                _schedule.Item(round) |> Array.Parallel.iter (fun se -> executeAndReschedule se)
//                                _schedule <- _schedule.Remove(round)
//                        | Get replyChannel ->
//                            replyChannel.Reply(_schedule)
//                        | Init map ->
//                            _schedule <- map
//                }
//            )
//    member _.AddToSchedule round scheduledEvent = agent.Post (AddToSchedule (round,scheduledEvent))
//    member _.ExecuteScheduled round = agent.Post (ExecuteScheduled round)
//    member _.Init map = agent.Post (Init map)
//    member _.Get = agent.PostAndReply Get
//    member _.PendingUpdates = agent.CurrentQueueLength > 0


