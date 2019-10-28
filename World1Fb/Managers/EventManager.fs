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
                            while (inbox.CurrentQueueLength > 0 || enm.PendingUpdates) do // I don't think this is needed
                                //printfn "Queue length:%i. Pending:%b" (inbox.CurrentQueueLength) enm.PendingUpdates
                                System.Threading.Thread.Sleep 1 // I think this only happens at the start, maybe log it. 0000 //Long delay so hopefully I will notice
                            resultAgent.Post EndOfRound
                            _round <- _round + 1u
                            replyChannel.Reply(_round)
                        | GetRound replyChannel -> 
                            replyChannel.Reply(_round)
                }
            )

    member this.GetEventLog = 
        resultAgent.PostAndReply Get

    member this.ProcessEvents = 
        //let st = Timer.Start random.Next
        //printfn "%A" st
        
        while callbackAgent.CurrentQueueLength > 0 do
            System.Threading.Thread.Sleep 1
        
        callbackAgent.PostAndReply EndRound
        
        //Timer.End "Process Events2" st

    member this.QueueEvent (ge:EventData_Generic) = 
        match _listeners.ContainsKey ge.GameEventType with 
        | false -> ()
        | true -> 
            _listeners.Item ge.GameEventType 
            |> Array.Parallel.iter (fun cb -> callbackAgent.Post (Callback (cb,ge)))

    member this.RegisterListener et callback = 
        _listeners <- Map_AppendValueToArray _listeners et callback
    
    member this.Round = callbackAgent.PostAndReply GetRound
