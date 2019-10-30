module ComponentEntityAgent
open AbstractComponent
open CommonGenericFunctions


type private ComponentEntityAgentMsg = 
| Add of Component
| Get of int * AsyncReplyChannel<uint32[]>
| GetMap of AsyncReplyChannel<Map<int,uint32[]> >
| Init of Map<int,uint32[]>    
| Remove of Component


type ComponentEntityAgent() = 
    let agent =
        let mutable _map = Map.empty<int,uint32[]>
        MailboxProcessor<ComponentEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add ct ->
                            match _map.ContainsKey ct.ComponentID with
                            | false -> _map <- _map.Add(ct.ComponentID,[|ct.EntityID|])
                            | true -> 
                                if not (_map.Item(ct.ComponentID)|>Array.contains ct.EntityID) then
                                    _map <- Map_AppendValueToArray _map ct.ComponentID ct.EntityID
                        | Get (cid,replyChannel) -> 
                            replyChannel.Reply(_map.Item(cid))
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
    
    member this.Get (cid:int) = 
        agent.PostAndReply (fun replyChannel -> Get (cid,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap
        
    member this.Init (newMap:Map<int,uint32[]>) =
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

 
 