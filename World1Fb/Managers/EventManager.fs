module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes

type private GECallback = EntityManager -> EventData_Generic -> Result<string option,string>

type private GECallbackExecution = GECallback * EventData_Generic

type GECallbackResult = uint32 * GECallback * EventData_Generic * Result<string option,string>

type private resultAgentMsg =
| EndOfRound
| Get of AsyncReplyChannel<GECallbackResult[]>
| Result of GECallbackResult

type private callbackAgentMsg =
| Callback of GECallbackExecution
| EndRound of AsyncReplyChannel<uint32>
| EndRound2
| GetRound of AsyncReplyChannel<uint32>

type EventManager(enm:EntityManager) =
    let mutable _listeners = Map.empty:Map<GameEventTypes,GECallback[]>

    let resultAgent =
        let mutable _log = Array.empty<GECallbackResult>
        MailboxProcessor<resultAgentMsg>.Start(
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

    let callbackAgent =
        let mutable _round = 0u
        MailboxProcessor<callbackAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with 
                        | Callback (cb,ge) -> 
                            resultAgent.Post (Result (_round,cb,ge,cb enm ge))
                        | EndRound replyChannel -> 
                            while (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) do
                                System.Threading.Thread.Sleep 2
                            resultAgent.Post EndOfRound
                            _round <- _round + 1u
                            replyChannel.Reply(_round)
                        | EndRound2 -> 
                            while (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) do
                                System.Threading.Thread.Sleep 2
                            //resultAgent.Post EndOfRound
                            _round <- _round + 1u
                        | GetRound replyChannel -> 
                            replyChannel.Reply(_round)
                }
            )

    member this.EndRound = 
        callbackAgent.PostAndReply EndRound 

    member this.EndRound2 = 
        callbackAgent.Post EndRound2 

    member this.GetEventLog = 
        resultAgent.PostAndReply Get

    member this.PrintEventLog =
        this.GetEventLog |> Array.iter (fun (rnd,cb,ge,res) -> if rnd <> 0u then printfn "%i | %A / %s" rnd (ge.GameEventType) (res.ToString()))
    
    member this.QueueEvent (ge:EventData_Generic) = 
        match _listeners.ContainsKey ge.GameEventType with 
        | false -> ()
        | true -> 
            _listeners.Item ge.GameEventType 
            |> Array.Parallel.iter (fun cb -> callbackAgent.Post (Callback (cb,ge)))

    member this.RegisterListener et callback = 
        _listeners <- Map_AppendValueToArray _listeners et callback
    
    member this.Round() = 
        callbackAgent.PostAndReply GetRound


