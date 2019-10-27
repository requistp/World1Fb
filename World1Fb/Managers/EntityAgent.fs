module EntityAgent


type EntityAgentMsg = 
    | Add of uint32
    | Init of uint32[]
    | Get of AsyncReplyChannel<uint32[]>
    | Remove of uint32


type EntityAgent(eids:uint32[]) =

    let agent =
        let mutable _array = eids

        MailboxProcessor<EntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add eid -> 
                            if not (_array|>Array.contains eid) then 
                                _array <- [|eid|] |> Array.append _array
                        | Get replyChannel -> 
                            replyChannel.Reply(_array)
                        | Init eids -> 
                            _array <- eids
                        | Remove eid ->
                            if (_array|>Array.contains eid) then 
                                _array <- _array |> Array.filter (fun e -> e <> eid)
                }
            )
    
    member this.Add eid = agent.Post (Add eid)

    member this.Init eids = agent.Post (Init eids)

    member this.Get = agent.PostAndReply Get 

    member this.Remove eid = agent.Post (Remove eid)

    new () = EntityAgent(Array.empty)

