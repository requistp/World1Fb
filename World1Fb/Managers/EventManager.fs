module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes
open Logging
open System

type private GECallback = EventData_Generic -> Result<string option,string>

type private GECallbackResult = string * GECallback * EventData_Generic * Result<string option,string> 

type private agentLogTypes =
    | CallbackResult of GECallbackResult
    | EndOfRoundCancelled
    | ListenerRegistered of string
    | NoListeners of EventData_Generic
    override this.ToString() = 
        match this with
        | CallbackResult (listener,cb,ge,res) -> 
            let res_ToStrings =
                match res with
                | Error x -> ("Err", " : " + x)
                | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
            sprintf "%-3s | %-20s -> %-30s%s" (fst res_ToStrings) listener (ge.ToString) (snd res_ToStrings)
        | EndOfRoundCancelled ->
            sprintf "End of round cancelled pending more events"
        | ListenerRegistered s -> 
            sprintf "%-3s | %-20s -> %-30s" "Ok " "Registered System" s
        | NoListeners ge -> 
            sprintf "%-3s | %-20s -> %-30s" " * " "<none>" (ge.ToString)

type private agentLogMsg =
    | EndOfRound
    | EndOfRound_Cancelled
    | Log of agentLogTypes
    | SetLogging of bool

type private agentCallbackMsg =
    | Callback of string * GECallback * EventData_Generic
    | EndRound of cancelled:AsyncReplyChannel<bool>
    | GetRound of AsyncReplyChannel<uint32>

type private agentListenersMsg =
    | Execute of EventData_Generic 
    | Register of string * GameEventTypes * GECallback

type EventManager(enm:EntityManager) =
    let mutable _round = 0u
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
                            if (_logging) then _log |> Array.iter (fun (r,lt) -> writeLog (sprintf "%7i | %s" r (lt.ToString())))
                            _log <- Array.empty
                        | EndOfRound_Cancelled ->
                            _log <- [| _round,EndOfRoundCancelled |] |> Array.append _log
                        | Log result -> 
                            _log <- [| _round,result |] |> Array.append _log 
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
                                _round <- _round + 1u
                                replyChannel.Reply(false)
                        | GetRound replyChannel -> 
                            replyChannel.Reply(_round)
                }
            )
    let agentListeners =
        let mutable _listeners = Map.empty:Map<GameEventTypes,(string*GECallback)[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Execute ge ->
                            match _listeners.ContainsKey ge.GameEventType with
                            | false -> 
                                agentLog.Post (Log (NoListeners ge))
                            | true ->
                                _listeners.Item ge.GameEventType
                                |> Array.Parallel.iter (fun (l,cb) -> agentCallback.Post (Callback (l,cb,ge)))
                        | Register (l,et,cb) -> 
                            _listeners <- Map_AppendValueToArray _listeners et (l,cb)
                            agentLog.Post (Log (ListenerRegistered l))
                }
            )

    member _.EndRound =
        while (agentCallback.CurrentQueueLength > 0 || enm.PendingUpdates || agentCallback.PostAndReply EndRound) do
            Console.Write '.'
            System.Threading.Thread.Sleep 1
        agentCallback.PostAndReply GetRound

    member _.QueueEvent (ge:EventData_Generic) = 
        agentListeners.Post (Execute ge)

    member _.RegisterListener (listener:string) (et:GameEventTypes) (callback:GECallback) = 
        agentListeners.Post (Register (listener,et,callback))
    
    member _.SetLogging (toggle:bool) = 
        agentLog.Post (SetLogging toggle)

