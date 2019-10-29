module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes
open Logging
open System

type private GECallback = EventData_Generic -> Result<string option,string>

type private GECallbackResult = uint32 * string * GECallback option * EventData_Generic option * Result<string option,string> 

type private agentResultMsg =
| DumpLog
| EndOfRound
| EndOfRoundCancelled
| Get of AsyncReplyChannel<GECallbackResult[]>
| Result of GECallbackResult
| SetLogging of bool

type private agentCallbackMsg =
| Callback of string * GECallback * EventData_Generic
| EndRound of AsyncReplyChannel<uint32>

type private agentListenersMsg =
| Execute of EventData_Generic 
| Register of string * GameEventTypes * GECallback

type EventManager(enm:EntityManager) =
    let mutable _round = 0u
    let agentResult =
        let mutable _log = Array.empty<GECallbackResult>
        let mutable _logging = true
        MailboxProcessor<agentResultMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let GECallbackResult_ToString ((rnd,l,cb,ge,res):GECallbackResult) =
                            let res_ToStrings =
                                match res with
                                | Error x -> ("Err", " : " + x)
                                | Ok s -> ("Ok", if s.IsSome then " : " + s.Value else "")
                            sprintf "%5i | %-3s | %-20s -> %-25s%s" rnd (fst res_ToStrings) l (if ge.IsSome then ge.Value.ToString else "<none>") (snd res_ToStrings)
                        let! msg = inbox.Receive()
                        match msg with
                        | DumpLog ->
                            _log |> Array.iter (fun gecb -> log (GECallbackResult_ToString gecb))
                            _log <- Array.empty
                        | EndOfRound ->
                            if (_logging) then _log |> Array.iter (fun gecb -> log (GECallbackResult_ToString gecb))
                            _log <- Array.empty
                        | EndOfRoundCancelled ->
                            _log <- Array.append _log [| ( _round, "EventManager", None, None, Error "Pending Events" )|]
                        | Get replyChannel ->
                            replyChannel.Reply(_log)
                            _log <- Array.empty
                        | SetLogging b ->
                            _logging <- b
                        | Result r -> 
                            _log <- Array.append _log [|r|]
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
                            agentResult.Post (Result (_round,l,Some cb,Some ge,cb ge))
                        | EndRound replyChannel -> 
                            match (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) with
                            | true -> 
                                // If I want to log this... agentResult.Post EndOfRoundCancelled
                                replyChannel.Reply(0u)
                            | false -> 
                                agentResult.Post EndOfRound
                                _round <- _round + 1u
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
                                agentResult.Post (Result (_round,"<nolisteners>",None,Some ge,Ok None))
                            | true ->
                                _listeners.Item ge.GameEventType
                                |> Array.Parallel.iter (fun (l,cb) -> agentCallback.Post (Callback (l,cb,ge)))
                        | Register (l,et,cb) -> 
                            _listeners <- Map_AppendValueToArray _listeners et (l,cb)
                }
            )

    member this.DumpLog = 
        agentResult.Post DumpLog

    member this.EndRound = 
        while (agentCallback.CurrentQueueLength > 0 || enm.PendingUpdates || agentCallback.PostAndReply EndRound = 0u) do
            Console.Write '.'
            System.Threading.Thread.Sleep 1
        _round

    member this.GetEventLog = 
        agentResult.PostAndReply Get

    member this.SetLogging (toggle:bool) = 
        agentResult.Post (SetLogging toggle)

    member this.QueueEvent (ge:EventData_Generic) = 
        agentListeners.Post (Execute ge)

    member this.RegisterListener (listener:string) (et:GameEventTypes) (callback:GECallback) = 
        agentListeners.Post (Register (listener,et,callback))
    
    //member this.PrintEventLog =
    //    this.GetEventLog |> Array.iter (fun (rnd,l,cb,ge,res) -> if rnd <> 0u then printfn "%i | %A / %s" rnd (ge.GameEventType) (res.ToString()))
    
