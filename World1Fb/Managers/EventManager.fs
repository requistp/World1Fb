module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes
open Logging
open System

type private GECallback = GameEventTypes -> Result<string option,string>

type private GECallbackResult = string * GECallback * GameEventTypes * Result<string option,string> 

type private agentLogTypes =
    | CallbackResult of GECallbackResult
    | EndOfRoundCancelled
    | ListenerRegistered of string
    | NoListeners of GameEventTypes
    | ScheduledEvent of GameEventTypes
    override this.ToString() = 
        match this with
        | CallbackResult (listener,cb,ge,res) -> 
            let res_ToStrings =
                match res with
                | Error x -> ("Err", " : " + x)
                | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
            sprintf "%-3s | %-20s -> %-30s%s" (fst res_ToStrings) listener (ge.GameEventType()) (snd res_ToStrings)
        | EndOfRoundCancelled ->
            sprintf "End of round cancelled pending more events"
        | ListenerRegistered s -> 
            sprintf "%-3s | %-20s -> %-30s" "Ok " "Registered System" s
        | NoListeners ge -> 
            sprintf "%-3s | %-20s -> %-30s" " * " "<none>" (ge.GameEventType())
        | ScheduledEvent se ->
            let sed,ge = se.ToScheduleEvent
            sprintf "%-3s | %-20s -> %-30s : Frequency:%i" "-->" "Scheduled Event" (ge.GameEventType()) sed.Frequency

type private agentLogMsg =
    | EndOfRound
    | EndOfRound_Cancelled
    | Log of agentLogTypes
    | SetLogging of bool

type private agentCallbackMsg =
    | Callback of string * GECallback * GameEventTypes
    | EndRound of cancelled:AsyncReplyChannel<bool>

type private agentListenersMsg =
    | Execute of GameEventTypes 
    | Register of string * byte * GECallback

type private agent_ScheduleMsg =
    | ExecuteScheduled of round:uint32 
    | Schedule of round:uint32 * GameEventTypes

type agentRoundMsg =
    | Get of AsyncReplyChannel<uint32>
    | Increment

type EventManager(enm:EntityManager) =
    let agentRound =
        let mutable _round = 0u
        MailboxProcessor<agentRoundMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Get replyChannel ->
                            replyChannel.Reply(_round)
                        | Increment ->
                            _round <- _round + 1u
                }
            )
    let agentLog =
        let mutable _log = Array.empty<uint32*agentLogTypes>
        let mutable _logging = true
        MailboxProcessor<agentLogMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | EndOfRound ->
                            agentRound.Post Increment
                            if (_logging) then _log |> Array.iter (fun (r,lt) -> writeLog (sprintf "%7i | %s" r (lt.ToString())))
                            _log <- Array.empty
                        | EndOfRound_Cancelled ->
                            _log <- [| agentRound.PostAndReply Get,EndOfRoundCancelled |] |> Array.append _log
                        | Log result -> 
                            _log <- [| agentRound.PostAndReply Get,result |] |> Array.append _log 
                        | SetLogging b ->
                            _logging <- b
                }
            )
    let agentCallback =
        MailboxProcessor<agentCallbackMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Callback (l,cb,ge) -> 
                            agentLog.Post (Log (CallbackResult (l,cb,ge,cb ge)))
                        | EndRound replyChannel -> 
                            match (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) with
                            | true -> 
                                // If I want to log this... agentLog.Post (EndOfRound_Cancelled _round)
                                replyChannel.Reply(true)
                            | false -> 
                                agentLog.Post EndOfRound
                                replyChannel.Reply(false)
                }
            )
    let agentListeners =
        let mutable _listeners = Map.empty:Map<byte,(string*GECallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute ge ->
                            match _listeners.ContainsKey ge.GameEventID with
                            | false -> 
                                agentLog.Post (Log (NoListeners ge))
                            | true ->
                                _listeners.Item ge.GameEventID
                                |> Array.Parallel.iter (fun (l,cb) -> agentCallback.Post (Callback (l,cb,ge)))
                        | Register (l,et,cb) -> 
                            _listeners <- Map_AppendValueToArray _listeners et (l,cb)
                            agentLog.Post (Log (ListenerRegistered l))
                }
            )
    let agentSchedule =
        let mutable _schedule = Map.empty<uint32,GameEventTypes[]>
        MailboxProcessor<agent_ScheduleMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let addToSchedule r (se:GameEventTypes) isNew =
                            let sed,_ = se.ToScheduleEvent
                            let interval = 
                                match isNew with
                                | false -> sed.Frequency
                                | true -> sed.Frequency //uint32 (TimingOffset (int sed.Frequency))
                            _schedule <- Map_AppendValueToArray _schedule (r+interval) se
                            agentLog.Post (Log (ScheduledEvent se))
                        match msg with
                        | ExecuteScheduled round ->
                            let executeAndReschedule (se:GameEventTypes) =
                                let _,ge = se.ToScheduleEvent
                                match enm.Exists ge.EntityID with
                                | false -> ()
                                | true ->
                                    agentListeners.Post (Execute ge)
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

    member _.EndRound =
        while (agentCallback.CurrentQueueLength > 0 || agentListeners.CurrentQueueLength > 0 || agentSchedule.CurrentQueueLength > 0 || enm.PendingUpdates || agentCallback.PostAndReply EndRound) do
            Console.Write '.'
            System.Threading.Thread.Sleep 1

    member _.GetRound() =
        agentRound.PostAndReply Get

    member _.QueueEvent (ge:GameEventTypes) = 
        agentListeners.Post (Execute ge)

    member _.ScheduleEvent (se:GameEventTypes) = 
        agentSchedule.Post (Schedule (agentRound.PostAndReply Get,se))
    
    member _.ExecuteScheduledEvents = 
        agentSchedule.Post (ExecuteScheduled (agentRound.PostAndReply Get))
        while (agentCallback.CurrentQueueLength > 0 || agentListeners.CurrentQueueLength > 0 || agentSchedule.CurrentQueueLength > 0 || enm.PendingUpdates) do
            Console.Write '!'
            System.Threading.Thread.Sleep 1

    member _.RegisterListener (listener:string) (et:byte) (callback:GECallback) = 
        agentListeners.Post (Register (listener,et,callback))
    
    member _.SetLogging (toggle:bool) = 
        agentLog.Post (SetLogging toggle)

