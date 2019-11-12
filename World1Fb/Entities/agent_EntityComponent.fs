﻿module agent_EntityComponent
open Component


type private agent_EntityComponentMsg = 
| Add of Component[]
| Exists of uint32 * AsyncReplyChannel<bool>
| GetComponents of uint32 * AsyncReplyChannel<Component[]>
| GetAll of AsyncReplyChannel<Map<uint32,Component[]> >
| Init of Map<uint32,Component[]>
| Remove of uint32
| ReplaceComponent of Component


type agent_EntityComponent() = 
    let agent =
        let mutable _ecmap = Map.empty<uint32,Component[]>
        MailboxProcessor<agent_EntityComponentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add cts ->
                            if not (_ecmap.ContainsKey(cts.[0].EntityID)) then 
                                _ecmap <- _ecmap.Add(cts.[0].EntityID,cts)
                        | Exists (eid,replyChannel) ->
                            replyChannel.Reply(_ecmap.ContainsKey eid)
                        | GetComponents (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _ecmap.ContainsKey eid with
                                | false -> [||]
                                | true -> _ecmap.Item(eid))
                        | GetAll replyChannel ->
                            replyChannel.Reply(_ecmap)
                        | Init map -> 
                            _ecmap <- map
                        | Remove eid -> 
                            _ecmap <- _ecmap.Remove eid
                        | ReplaceComponent (c:Component) ->
                            if (_ecmap.ContainsKey c.EntityID) then
                                _ecmap <-
                                    let a = 
                                        _ecmap.Item(c.EntityID)
                                        |> Array.filter (fun ac -> ac.ComponentID <> c.ComponentID)
                                        |> Array.append [|c|]
                                    _ecmap.Remove(c.EntityID).Add(c.EntityID,a)
                }
            )

    member _.CreateEntity (cts:Component[]) = agent.Post (Add cts)

    member _.Exists (eid:uint32) = agent.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member _.GetAll() = agent.PostAndReply GetAll

    member _.GetComponents (eid:uint32) = agent.PostAndReply (fun replyChannel -> GetComponents(eid,replyChannel))

    member _.Init (map:Map<uint32,Component[]>) = agent.Post (Init map)

    member _.PendingUpdates = agent.CurrentQueueLength > 0

    member _.RemoveEntity (eid:uint32) = agent.Post (Remove eid)

    member _.ReplaceComponent (c:Component) = agent.Post (ReplaceComponent c)


