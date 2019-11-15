module agent_ComponentEntity
//open Component
//open CommonGenericFunctions


//type private agent_ComponentEntityMsg = 
//    | AddComponent of Component
//    | GetEntitiesWithComponent of componentID:byte * entityID:AsyncReplyChannel<uint32[]>
//    | GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
//    | InitComponents of Map<byte,uint32[]>    
//    | RemoveComponent of Component


//type agent_ComponentEntity() = 
//    let agentComponents =
//        let mutable _map = Map.empty<byte,uint32[]>
//        MailboxProcessor<agent_ComponentEntityMsg>.Start(
//            fun inbox ->
//                async { 
//                    while true do
//                        let! msg = inbox.Receive()
//                        match msg with
//                        | AddComponent c ->
//                            match _map.ContainsKey c.ComponentID with
//                            | false -> _map <- _map.Add(c.ComponentID,[|c.EntityID|])
//                            | true -> 
//                                _map <- Map_AppendValueToArrayUnique _map c.ComponentID c.EntityID 
//                        | GetEntitiesWithComponent (cid,replyChannel) -> 
//                            replyChannel.Reply(
//                                match _map.ContainsKey(cid) with
//                                | false -> Array.empty
//                                | true -> _map.Item(cid))
//                        | GetComponentMap replyChannel -> 
//                            replyChannel.Reply(_map)
//                        | InitComponents newMap -> 
//                            _map <- newMap
//                        | RemoveComponent ct ->
//                            _map <- Map_RemoveValueFromArray _map ct.ComponentID ct.EntityID
//                }
//            )
//    member _.AddComponentsToMap (cts:Component[]) = cts |> Array.Parallel.iter (fun ct -> agentComponents.Post (AddComponent ct))
//    member _.GetEntitiesWithComponent (componentID:byte) = agentComponents.PostAndReply (fun replyChannel -> GetEntitiesWithComponent (componentID,replyChannel))
//    member _.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
//    member _.RemoveComponentsFromMap (cts:Component[]) = cts |> Array.Parallel.iter (fun ct -> agentComponents.Post (RemoveComponent ct))

 
 