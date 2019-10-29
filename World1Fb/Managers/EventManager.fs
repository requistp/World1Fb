module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes
open System

type private GECallback = EntityManager -> EventData_Generic -> Result<string option,string>

type private GECallbackResult = uint32 * GECallback * EventData_Generic * Result<string option,string>

type private agentResultMsg =
| EndOfRound
| Get of AsyncReplyChannel<GECallbackResult[]>
| Result of GECallbackResult

type private agentCallbackMsg =
| Callback of GECallback * EventData_Generic
| EndRound of AsyncReplyChannel<uint32>

type private agentListenersMsg =
| Add of GameEventTypes * GECallback
| Execute of EventData_Generic 

type EventManager(enm:EntityManager) =
    let agentResult =
        let mutable _log = Array.empty<GECallbackResult>
        MailboxProcessor<agentResultMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | EndOfRound ->
                            //_log |> Array.iter (fun (rnd,cb,ge,res) -> if rnd <> 0u then printfn "%i | %A / %s" rnd (ge.GameEventType) (res.ToString()))
                            //_log <- Array.empty
                            ()
                        | Get replyChannel ->
                            replyChannel.Reply(_log)
                            _log <- Array.empty
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
                        | Callback (cb,ge) -> 
                            agentResult.Post (Result (_round,cb,ge,cb enm ge))
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
        let mutable _listeners = Map.empty:Map<GameEventTypes,GECallback[]>
        MailboxProcessor<agentListenersMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Add (et,cb) -> 
                            _listeners <- Map_AppendValueToArray _listeners et cb
                        | Execute ge ->
                            if (_listeners.ContainsKey ge.GameEventType) then
                                _listeners.Item ge.GameEventType
                                |> Array.Parallel.iter (fun cb -> agentCallback.Post (Callback (cb,ge)))
                }
            )

    member this.EndRound = 
        while (agentCallback.CurrentQueueLength > 0 || enm.PendingUpdates) do
            Console.Write '.'
            System.Threading.Thread.Sleep 1
        agentCallback.PostAndReply EndRound 

    member this.GetEventLog = 
        agentResult.PostAndReply Get

    member this.QueueEvent (ge:EventData_Generic) = 
        agentListeners.Post (Execute ge)

    member this.RegisterListener et callback = 
        agentListeners.Post (Add (et,callback))
    
    member this.PrintEventLog =
        this.GetEventLog |> Array.iter (fun (rnd,cb,ge,res) -> if rnd <> 0u then printfn "%i | %A / %s" rnd (ge.GameEventType) (res.ToString()))
    
