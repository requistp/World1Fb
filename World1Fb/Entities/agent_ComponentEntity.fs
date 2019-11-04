module agent_ComponentEntity
open Component
open CommonGenericFunctions


type private agent_ComponentEntityMsg = 
| Add of Component
| Get of byte * AsyncReplyChannel<uint32[]>
| Init of Map<byte,uint32[]>    
| Remove of Component


type agent_ComponentEntity() = 
    let agent =
        let mutable _map = Map.empty<byte,uint32[]>
        MailboxProcessor<agent_ComponentEntityMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add c ->
                            match _map.ContainsKey c.ComponentID with
                            | false -> _map <- _map.Add(c.ComponentID,[|c.EntityID|])
                            | true -> 
                                _map <- Map_AppendValueToArrayUnique _map c.ComponentID c.EntityID 
                        | Get (cid,replyChannel) -> 
                            replyChannel.Reply(
                                match _map.ContainsKey(cid) with
                                | false -> Array.empty
                                | true -> _map.Item(cid))
                        | Init newMap -> 
                            _map <- newMap
                        | Remove ct ->
                            _map <- Map_RemoveValueFromArray _map ct.ComponentID ct.EntityID
                }
            )

    member _.Add (ct:Component) = agent.Post (Add ct)

    member me.Add (cts:Component[]) = 
        cts |> Array.Parallel.iter (fun ct -> me.Add ct)
    
    member _.Get (cid:byte) = agent.PostAndReply (fun replyChannel -> Get (cid,replyChannel))

    member _.Init (newMap:Map<byte,uint32[]>) = agent.Post (Init newMap)
    
    member _.PendingUpdates = agent.CurrentQueueLength > 0

    member _.Remove (ct:Component) = agent.Post (Remove ct)

    member me.Remove (cts:Component[]) = 
        cts |> Array.Parallel.iter (fun ct -> me.Remove ct)

 
 