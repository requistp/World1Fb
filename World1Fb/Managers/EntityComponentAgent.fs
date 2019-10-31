module EntityComponentAgent
open Component


type private EntityComponentAgentMsg = 
| AddEntity of uint32 * Component[]
| Exists of uint32 * AsyncReplyChannel<bool>
| GetComponents of uint32 * AsyncReplyChannel<Component[]>
| GetMap of AsyncReplyChannel< Map<uint32,Component[]> >
| Init of Map<uint32,Component[]>
| RemoveEntity of uint32
| ReplaceComponent of Component


type EntityComponentAgent() = 
    let agent =
        let mutable _next = Map.empty<uint32,Component[]>
        MailboxProcessor<EntityComponentAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntity (e,cts) ->
                            if not (_next.ContainsKey(e)) then 
                                _next <- _next.Add(e,cts)
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
                        | ReplaceComponent (c:Component) ->
                            if (_next.ContainsKey c.EntityID) then
                                _next <-
                                    let a = 
                                        _next.Item(c.EntityID)
                                        |> Array.filter (fun ac -> ac.ComponentID <> c.ComponentID)
                                        |> Array.append [|c|]
                                    _next.Remove(c.EntityID).Add(c.EntityID,a)
                }
            )

    member this.CreateEntity (e,cts:Component[]) = 
        agent.Post (AddEntity (e,cts))

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

    member this.ReplaceComponent (c:Component) =
        agent.Post (ReplaceComponent c)

    member this.Init (newMap:Map<uint32,Component[]>) = 
        agent.Post (Init newMap)


