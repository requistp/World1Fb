module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes
open Logging
open System

type private GECallback = EventData_Generic -> Result<string option,string>

type private GECallbackResult = uint32 * string * GECallback * EventData_Generic * Result<string option,string> 

type private agentResultMsg =
| DumpLog
| EndOfRound
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
                                | Error x -> ("Err",x)
                                | Ok s -> ("Ok ",if s.IsSome then " : " + s.Value else ".")
                            sprintf "%5i | %s | %s by %s%s" rnd (fst res_ToStrings) (ge.GameEventType.ToString()) l (snd res_ToStrings)
                        let! msg = inbox.Receive()
                        match msg with
                        | DumpLog ->
                            _log |> Array.iter (fun gecb -> log (GECallbackResult_ToString gecb))
                            _log <- Array.empty
                        | EndOfRound ->
                            if (_logging) then _log |> Array.iter (fun gecb -> log (GECallbackResult_ToString gecb))
                            _log <- Array.empty
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
        let mutable _round = 0u
        MailboxProcessor<agentCallbackMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Callback (l,cb,ge) -> 
                            agentResult.Post (Result (_round,l,cb,ge,cb ge))
                        | EndRound replyChannel -> 
                            while (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) do
                                Console.Write '!'
                                System.Threading.Thread.Sleep 1
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
                            if (_listeners.ContainsKey ge.GameEventType) then
                                _listeners.Item ge.GameEventType
                                |> Array.Parallel.iter (fun (l,cb) -> agentCallback.Post (Callback (l,cb,ge)))
                        | Register (l,et,cb) -> 
                            _listeners <- Map_AppendValueToArray _listeners et (l,cb)
                }
            )

    member this.DumpLog = 
        agentResult.Post DumpLog

    member this.EndRound = 
        while (agentCallback.CurrentQueueLength > 0 || enm.PendingUpdates) do
            Console.Write '.'
            System.Threading.Thread.Sleep 1
        agentCallback.PostAndReply EndRound 

    member this.GetEventLog = 
        agentResult.PostAndReply Get

    member this.SetLogging (toggle:bool) = 
        agentResult.Post (SetLogging toggle)

    member this.QueueEvent (ge:EventData_Generic) = 
        agentListeners.Post (Execute ge)

    member this.RegisterListener (listener:string) (et:GameEventTypes) (callback:GECallback) = 
        agentListeners.Post (Register (listener,et,callback))
    
    member this.PrintEventLog =
        this.GetEventLog |> Array.iter (fun (rnd,l,cb,ge,res) -> if rnd <> 0u then printfn "%i | %A / %s" rnd (ge.GameEventType) (res.ToString()))
    
