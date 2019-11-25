﻿module EventManager
open agent_GameLog
open CalendarTimings
open CommonGenericFunctions
open EntityManager
open EventTypes

type GameEventCallback = RoundNumber -> GameEventData -> Result<string option,string>

type private agent_ScheduleMsg =
    | AddToSchedule of RoundNumber * ScheduledEventData
    | ExecuteScheduled of RoundNumber 
    | Init of Map<RoundNumber,ScheduledEventData[]>
    | Get  of AsyncReplyChannel<Map<RoundNumber,ScheduledEventData[]> >

type private agentListenersMsg =
    | Execute of RoundNumber * GameEventData 
    | Register of listener:string * GameEventTypes * GameEventCallback

type EventManager(enm:EntityManager, log:agent_GameLog, getRound:unit->RoundNumber) =
    let agentListeners =
        let mutable _listeners = Map.empty:Map<GameEventTypes,(string*GameEventCallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute (round,gameEvent) ->
                            match _listeners.ContainsKey(GetGameEvent gameEvent) with
                            | false -> 
                                log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i" " * " "<none>" ((GetGameEvent gameEvent).ToString()) (GetGameEvent_EntityID gameEvent).ToUint32)
                            | true ->
                                _listeners.Item(GetGameEvent gameEvent)
                                |> Array.Parallel.iter (fun (listener,callback) -> 
                                    let result = callback round gameEvent
                                    let res_ToStrings =
                                        match result with
                                        | Error x -> ("Err", " : " + x)
                                        | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
                                    log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i%s" (fst res_ToStrings) listener ((GetGameEvent gameEvent).ToString()) (GetGameEvent_EntityID gameEvent).ToUint32 (snd res_ToStrings)))
                        | Register (listener,eventType,callback) -> 
                            _listeners <- Map_AppendValueToArrayNonUnique _listeners eventType (listener,callback)
                            log.Log (RoundNumber(0u)) (sprintf "%-3s | %-20s -> %-30s" "Ok " "Registered System" listener)
                }
            )
    
    let agentSchedule =
        let mutable _schedule = Map.empty<RoundNumber,ScheduledEventData[]>
        MailboxProcessor<agent_ScheduleMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let addToSchedule (round:RoundNumber) (se:ScheduledEventData) isNew =
                            let interval = 
                                match isNew && se.ScheduleType = RepeatIndefinitely with
                                | true -> TimingOffset se.Frequency
                                | false -> se.Frequency
                            _schedule <- Map_AppendValueToArrayNonUnique _schedule (round+interval) se
                            log.Log round (sprintf "%-3s | %-20s -> %-30s #%7i : Frequency:%i" "-->" "Scheduled Event" ((GetGameEvent se.GameEvent).ToString()) (GetGameEvent_EntityID se.GameEvent).ToUint32 se.Frequency.ToUint32)
                        match msg with
                        | AddToSchedule (round,se) -> 
                            addToSchedule round se true
                        | ExecuteScheduled round ->
                            let reschedule (se:ScheduledEventData) =
                                match se.ScheduleType with
                                | RunOnce -> ()
                                | RepeatIndefinitely -> addToSchedule round se false 
                                | RepeatFinite remaining -> if (remaining > 1) then addToSchedule round { se with ScheduleType = RepeatFinite (remaining - 1) } false
                            let executeAndReschedule (se:ScheduledEventData) =
                                if (enm.EntityExists (GetGameEvent_EntityID se.GameEvent)) then
                                    agentListeners.Post (Execute (round,se.GameEvent))
                                    reschedule se
                            if (_schedule.ContainsKey round) then
                                _schedule.Item(round) |> Array.Parallel.iter executeAndReschedule
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


