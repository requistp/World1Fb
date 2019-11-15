module agent_LocationEntity
//open Component
//open ComponentEnums
//open CommonGenericFunctions
//open FormComponent
//open LocationTypes


//type private agent_LocationsMsg =
//| AddForm of FormComponent
//| GetEntitiesAtLocation of LocationDataInt * AsyncReplyChannel<uint32[]>
//| GetLocationMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
//| InitLocations of Map<LocationDataInt,uint32[]>
//| MoveForm of oldForm:FormComponent * newForm:FormComponent
//| RemoveForm of FormComponent


//type agent_LocationEntity() = 
//    let agentLocations =
//        let mutable _map = MapLocations |> Array.fold (fun (m:Map<LocationDataInt,uint32[]>) l -> m.Add(l,Array.empty)) Map.empty
//        MailboxProcessor<agent_LocationsMsg>.Start(
//            fun inbox ->
//                async { 
//                    while true do
//                        let! msg = inbox.Receive()
//                        match msg with
//                        | AddForm fd ->
//                            _map <- Map_AppendValueToArrayUnique _map fd.Location fd.EntityID 
//                        | GetEntitiesAtLocation (location,replyChannel) -> 
//                            replyChannel.Reply(_map.Item(location))
//                        | GetLocationMap replyChannel -> 
//                            replyChannel.Reply(_map)
//                        | InitLocations newMap -> 
//                            _map <- newMap
//                        | MoveForm (oldForm,newForm) -> 
//                            _map <- Map_RemoveValueFromArray _map oldForm.Location oldForm.EntityID
//                            _map <- Map_AppendValueToArrayUnique _map newForm.Location newForm.EntityID 
//                        | RemoveForm fd ->
//                            _map <- Map_RemoveValueFromArray _map fd.Location fd.EntityID
//                }
//            )
//    member _.AddForm (fd:FormComponent) = agentLocations.Post (AddForm fd)
//    member me.AddForm (cts:Component[]) =
//        cts
//        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
//        |> Array.Parallel.iter (fun ct -> me.AddForm ct.ToForm)
//    member _.GetEntitiesAtLocation (location:LocationDataInt) = agentLocations.PostAndReply (fun replyChannel -> GetEntitiesAtLocation (location,replyChannel))
//    member _.GetLocationMap() = agentLocations.PostAndReply GetLocationMap
//    //member _.InitLocations (newMap:Map<LocationDataInt,uint32[]>) = agentLocations.Post (InitLocations newMap)
//    member _.MoveForm (oldForm:FormComponent) (newForm:FormComponent) = agentLocations.Post (MoveForm (oldForm,newForm))
//    //member _.PendingUpdates = agentLocations.CurrentQueueLength > 0
//    member _.RemoveForm (fd:FormComponent) = agentLocations.Post (RemoveForm fd)
//    member me.RemoveForm (cts:Component[]) =
        //cts
        //|> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        //|> Array.Parallel.iter (fun ct -> me.RemoveForm ct.ToForm)


