module ComponentEntityAgent
open Component
open CommonGenericFunctions


type private ComponentEntityAgentMsg = 
| Add of Component
| Get of byte * AsyncReplyChannel<uint32[]>
| GetMap of AsyncReplyChannel<Map<byte,uint32[]> >
| Init of Map<byte,uint32[]>    
| Remove of Component


type ComponentEntityAgent() = 
    let agent =
        let mutable _map = Map.empty<byte,uint32[]>
        MailboxProcessor<ComponentEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add c ->
                            match _map.ContainsKey c.ComponentID with
                            | false -> _map <- _map.Add(c.ComponentID,[|c.EntityID|])
                            | true -> 
                                if not (_map.Item(c.ComponentID)|>Array.contains c.EntityID) then
                                    _map <- Map_AppendValueToArray _map c.ComponentID c.EntityID
                        | Get (cid,replyChannel) -> 
                            replyChannel.Reply(
                                match _map.ContainsKey(cid) with
                                | false -> Array.empty
                                | true -> _map.Item(cid))
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | Init newMap -> 
                            _map <- newMap
                        | Remove ct ->
                            _map <-
                                let a = _map.Item(ct.ComponentID) |> Array.filter (fun eid -> eid <> ct.EntityID)
                                _map.Remove(ct.ComponentID).Add(ct.ComponentID,a)
                }
            )

    member this.Add (ct:Component) = 
        agent.Post (Add ct)

    member this.Add (cts:Component[]) =
        cts |> Array.Parallel.iter (fun ct -> this.Add ct)
    
    member this.Get (cid:byte) = 
        agent.PostAndReply (fun replyChannel -> Get (cid,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap
        
    member this.Init (newMap:Map<byte,uint32[]>) =
        agent.Post (Init newMap)
    
    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member this.Print =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.ToString()) v.Length)

    member this.Remove (ct:Component) =
        agent.Post (Remove ct)

    member this.Remove (cts:Component[]) =
        cts |> Array.Parallel.iter (fun ct -> this.Remove ct)

 
 