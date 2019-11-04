module agent_EntityComponent
open Component


type private agent_EntityComponentMsg = 
    | AddEntity of uint32 * Component[]
    | Exists of uint32 * AsyncReplyChannel<bool>
    | GetComponents of uint32 * AsyncReplyChannel<Component[]>
    | GetMap of AsyncReplyChannel< Map<uint32,Component[]> >
    | Init of Map<uint32,Component[]>
    | RemoveEntity of uint32
    | ReplaceComponent of Component


type agent_EntityComponent() = 
    let agent =
        let mutable _next = Map.empty<uint32,Component[]>
        MailboxProcessor<agent_EntityComponentMsg>.Start(
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

    member _.CreateEntity (e,cts:Component[]) = 
        agent.Post (AddEntity (e,cts))

    member _.Exists (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member _.GetComponents (eid:uint32) = 
        agent.PostAndReply (fun replyChannel -> GetComponents(eid,replyChannel))

    member _.GetMap() = 
        agent.PostAndReply GetMap

    member _.PendingUpdates = 
        agent.CurrentQueueLength > 0

    member _.RemoveEntity (eid:uint32) = 
        agent.Post (RemoveEntity eid)

    member _.ReplaceComponent (c:Component) =
        agent.Post (ReplaceComponent c)

    member _.Init (newMap:Map<uint32,Component[]>) = 
        agent.Post (Init newMap)


