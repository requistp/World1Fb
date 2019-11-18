module EventManager
open agent_GameLog
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes

type GameEventCallback = uint32 -> GameEventTypes -> Result<string option,string>

type private agent_ScheduleMsg =
    | AddToSchedule of round:uint32 * GameEventTypes
    | ExecuteScheduled of round:uint32 
    | Init of Map<uint32,GameEventTypes[]>
    | Get  of AsyncReplyChannel<Map<uint32,GameEventTypes[]> >

type private agentListenersMsg =
    | Execute of round:uint32 * gameEvent:GameEventTypes 
    | Register of listener:string * gameEventID:byte * GameEventCallback

type EventManager(enm:EntityManager, log:agent_GameLog, getRound:unit->uint32) =
    let agentListeners =
        let mutable _listeners = Map.empty:Map<byte,(string*GameEventCallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute (round,gameEvent) ->
                            match _listeners.ContainsKey gameEvent.GameEventID with
                            | false -> 
                                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i" " * " "<none>" (gameEvent.GameEventType()) gameEvent.EntityID)
                            | true ->
                                _listeners.Item gameEvent.GameEventID
                                |> Array.Parallel.iter (fun (listener,callback) -> 
                                    let result = callback round gameEvent
                                    let res_ToStrings =
                                        match result with
                                        | Error x -> ("Err", " : " + x)
                                        | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
                                    log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i%s" (fst res_ToStrings) listener (gameEvent.GameEventType()) gameEvent.EntityID (snd res_ToStrings)))
                        | Register (listener,eventType,callback) -> 
                            _listeners <- Map_AppendValueToArrayNonUnique _listeners eventType (listener,callback)
                            log.Log 0u (sprintf "%-3s | %-20s -> %-30s" "Ok " "Registered System" listener)
                }
            )
    let agentSchedule =
        let mutable _schedule = Map.empty<uint32,GameEventTypes[]>
        MailboxProcessor<agent_ScheduleMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let addToSchedule round (se:GameEventTypes) isNew =
                            let sed,ge = se.ToScheduleEvent
                            let interval = 
                                match isNew && sed.Schedule = RepeatIndefinitely with
                                | true -> uint32 (TimingOffset (int sed.Frequency))
                                | false -> sed.Frequency                                
                            _schedule <- Map_AppendValueToArrayNonUnique _schedule (round+interval) se
                            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : Frequency:%i" "-->" "Scheduled Event" (ge.GameEventType()) ge.EntityID sed.Frequency)
                        match msg with
                        | AddToSchedule (round,se) -> 
                            addToSchedule round se true
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
                                match enm.EntityExists (Some round) ge.EntityID with
                                | false -> ()
                                | true ->
                                    agentListeners.Post (Execute (round,ge))
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
                }
            )

    member _.AddToSchedule scheduledEvent = agentSchedule.Post (AddToSchedule (getRound(),scheduledEvent))
    member _.ExecuteScheduledEvents round = agentSchedule.Post (ExecuteScheduled (round))
    member _.Init map = agentSchedule.Post (Init map)
    member _.GetSchedule = agentSchedule.PostAndReply Get
    member _.PendingUpdates = agentSchedule.CurrentQueueLength > 0 || agentListeners.CurrentQueueLength > 0
    member _.RaiseEvent gameEvent = agentListeners.Post (Execute ((getRound()),gameEvent))
    member _.RegisterListener listener eventTypeID callback = agentListeners.Post (Register (listener,eventTypeID,callback))


