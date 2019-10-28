module EntityComponentAgent
open AbstractComponent


type private EntityComponentAgentMsg = 
| AddEntity of AbstractComponent[]
| Exists of uint32 * AsyncReplyChannel<bool>
| GetComponents of uint32 * AsyncReplyChannel<AbstractComponent[]>
| GetMap of AsyncReplyChannel< Map<uint32,AbstractComponent[]> >
| Init of Map<uint32,AbstractComponent[]>
| RemoveEntity of uint32
| ReplaceComponent of AbstractComponent


type EntityComponentAgent() = 
    let agent =
        let mutable _next = Map.empty<uint32,AbstractComponent[]>
        MailboxProcessor<EntityComponentAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntity cts ->
                            if not (_next.ContainsKey(cts.[0].EntityID)) then 
                                _next <- _next.Add(cts.[0].EntityID, cts)
                        | Exists (eid,replyChannel) ->
                            replyChannel.Reply(_next.ContainsKey eid)
                        | GetComponents (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _next.ContainsKey eid with
                                | false -> [||]
                                | true -> _next.Item(eid)
                            )
                        | GetMap replyChannel ->
                            replyChannel.Reply(_next)
                        | Init newMap -> 
                            _next <- newMap
                        | RemoveEntity eid -> 
                            _next <- _next.Remove eid
                        | ReplaceComponent c ->
                            if (_next.ContainsKey c.EntityID) then
                                _next <-
                                    let a = 
                                        _next.Item(c.EntityID)
                                        |> Array.filter (fun ac -> ac.ComponentType <> c.ComponentType)
                                        |> Array.append [|c|]
                                    _next.Remove(c.EntityID).Add(c.EntityID,a)
                }
            )

    member this.CreateEntity (cts:AbstractComponent[]) = 
        agent.Post (AddEntity cts)

    member this.Exists (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member this.GetComponents (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> GetComponents(eid,replyChannel))

    member this.GetMap() = 
        agent.PostAndReply GetMap

    member this.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member this.RemoveEntity (eid:uint32) = 
        agent.Post (RemoveEntity eid)

    member this.ReplaceComponent (ac:AbstractComponent) =
        agent.Post (ReplaceComponent ac)

    member this.Init (newMap:Map<uint32,AbstractComponent[]>) = 
        agent.Post (Init newMap)


//member this.List () =
//    _entDict |> Map.iter (fun k v -> printfn "%i | %i" k v.List.Length)
       


