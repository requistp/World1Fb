module EventManager
open CommonGenericFunctions
open EntityManager
open EventTypes

type GECallback = NextEntityDictionary -> EventData_Generic -> Result<string option,string>

type GECallbackExecution = GECallback option * EventData_Generic

type GECallbackResult = GECallback option * EventData_Generic * Result<string option,string>
    
type EventManager(enm:EntityManager, round:unit->uint32) =
    let mutable _listeners = Map.empty:Map<GameEventTypes,GECallback[]>
    
    let resultAgent =
        MailboxProcessor<GECallbackResult>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! cb,ge,res = inbox.Receive()
                            // Log round(), cb, ge, res someplace
                            //printfn "%s / %s / %s " (cb.ToString()) (ge.GameEventType.ToString()) (res.ToString())
                            //printfn "%i | %s %s" (round()) (ge.GameEventType.ToString()) (res.ToString())
                            ()
                    }
            )

    let callbackAgent =
        MailboxProcessor<GECallbackExecution>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! cb,ge = inbox.Receive()
                            match ge.GameEventType with 
                            | GAME_AdvanceRound -> resultAgent.Post (cb,ge,enm.SetToNext)
                            | _ -> 
                                match cb.IsSome with
                                | false -> resultAgent.Post (None,ge,Ok (Some "No listeners"))
                                | true -> resultAgent.Post (cb,ge,cb.Value enm.Next ge)
                    }
            )

    member this.ProcessEvents = 
        let st = Timer.Start random.Next
        printfn "%A" st
        while callbackAgent.CurrentQueueLength > 0 do
            System.Threading.Thread.Sleep 1
        callbackAgent.Post (None,EventData_Generic(GAME_AdvanceRound))
        while callbackAgent.CurrentQueueLength > 0 do
            System.Threading.Thread.Sleep 1
        Timer.End "Process Events2" st

    member this.QueueEvent (ge:EventData_Generic) = 
        match _listeners.ContainsKey ge.GameEventType with 
        | false -> ()
        | true -> 
            _listeners.Item ge.GameEventType 
            |> Array.Parallel.iter (fun cb -> callbackAgent.Post (Some cb,ge))

    member this.RegisterListener et callback = 
        _listeners <- Map_AppendValueToArray _listeners et callback
    
