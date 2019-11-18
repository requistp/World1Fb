module EntityManager
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent
open LocationTypes

//type historyTuple = Map<uint32,Component[]> * Map<byte,uint32[]> * Map<LocationDataInt,uint32[]>

//type private agent_HistoryMsg = 
//    | GetHistory of round:uint32 option * history:AsyncReplyChannel<historyTuple>
//    | GetAllHistory of allHistory:AsyncReplyChannel<Map<uint32,historyTuple> >
//    | InitHistory of allHistory:Map<uint32,historyTuple>
//    | RecordHistory of round:uint32 * history:historyTuple

//type private agent_ComponentsMsg = 
//    | AddComponent of Component
//    | GetEntitiesWithComponent of componentID:byte * entityID:AsyncReplyChannel<uint32[]>
//    | GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
//    | RemoveComponent of Component


type private agent_ComponentsMsg = 
    | CreateComponent of round:uint32 * comp:Component * compID:AsyncReplyChannel<int>
    | DeleteComponent of round:uint32 * compID:int
    | GetComponent of round:uint32 option * compID:int * comp:AsyncReplyChannel<Component option>
    //| InitComponents of (uint32*Component option)[]
    | UpdateComponent of round:uint32 * compID:int * comp:Component

type private agent_EntitiesMsg = 
    | CreateEntity of round:uint32 * entityID:uint32 * compIDs:int[]
    //| DeleteEntity of round:uint32 * entityID:uint32
    //| EntityExists of round:uint32 * entityID:uint32 * exists:AsyncReplyChannel<bool>
    | GetComponents of round:uint32 option * entityID:uint32 * componentIDsOption:AsyncReplyChannel<int[] option>
    | UpdateComponents of round:uint32 * entityID:uint32 * compIDs:int[]
    //| GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
    //| InitEntities of Map<uint32,Component[]>
    //| ReplaceComponent of componentID:int * comp:Component

type private agent_EntityIDMsg = 
    | GetID of maxEntityID:AsyncReplyChannel<uint32>
    | InitID of maxEntityID:uint32
    | NewID of AsyncReplyChannel<uint32>

type private agent_LocationsMsg =
    | AddEntityToLocation of location:LocationDataInt * round:uint32 * entityID:uint32
    | GetEntitiesAtLocation of round:uint32 option * location:LocationDataInt * entityIDs:AsyncReplyChannel<uint32[]>
    //| GetLocationMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
    //| RemoveForm of FormComponent
    | UpdateForm of round:uint32 * entityID:uint32 * oldForm:FormComponent * newForm:FormComponent

type EntityManager() = 
    let rec searchArrayDataForRound (round:uint32 option) (arrayToSearch:(uint32*'a option)[]) =
        match Array.head arrayToSearch with
        | r,c when round.IsNone || r <= round.Value -> c
        | _ -> 
            let t = Array.tail arrayToSearch
            match t with
            | [||] -> None
            | _ -> searchArrayDataForRound round t
    //let agentComponents =
    //    let mutable _map = Map.empty<byte,uint32[]>
    //    MailboxProcessor<agent_ComponentsMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    | AddComponent c ->
    //                        match _map.ContainsKey c.ComponentID with
    //                        | false -> _map <- _map.Add(c.ComponentID,[|c.EntityID|])
    //                        | true -> 
    //                            _map <- Map_AppendValueToArrayUnique _map c.ComponentID c.EntityID 
    //                    | GetEntitiesWithComponent (cid,replyChannel) -> 
    //                        replyChannel.Reply(
    //                            match _map.ContainsKey(cid) with
    //                            | false -> Array.empty
    //                            | true -> _map.Item(cid))
    //                    | GetComponentMap replyChannel -> 
    //                        replyChannel.Reply(_map)
    //                    | RemoveComponent ct ->
    //                        _map <- Map_RemoveValueFromArray _map ct.ComponentID ct.EntityID
    //            }
    //        )
    //let agentHistory =
    //    let mutable _history = Map.empty<uint32,historyTuple>
    //    MailboxProcessor<agent_HistoryMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    | GetHistory (round,replyChannel) ->
    //                        replyChannel.Reply(
    //                            match round with
    //                            | Some r -> _history.Item r
    //                            | None -> _history.Item (uint32 (_history.Count - 1))
    //                            )
    //                    | GetAllHistory replyChannel ->
    //                        replyChannel.Reply(_history)
    //                    | InitHistory hs ->
    //                        _history <- hs
    //                    | RecordHistory (round,h) -> 
    //                        _history <- _history.Add(round,h)
    //            }
    //        )
    let agent_Components =
        let mutable _array = Array.empty<(uint32*Component option)[]>
        MailboxProcessor<agent_ComponentsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | CreateComponent (round,c,replyChannel) ->
                            _array <- Array.append _array [| [| (round,Some c) |] |]
                            replyChannel.Reply(_array.Length - 1)
                        | DeleteComponent (r,cid) ->
                            match (snd _array.[cid].[0]).IsNone with
                            | true -> () // Should probable raise an error, already None
                            | false ->
                                _array.[cid] <- Array.append [|(r,None)|] _array.[cid]
                        | GetComponent (round,cid,replyChannel) ->
                            replyChannel.Reply(searchArrayDataForRound round _array.[cid])
                        | UpdateComponent (r,cid,c) ->
                            match (snd _array.[cid].[0]).IsNone with
                            | true -> () //Should probably raise an error
                            | false ->
                                _array.[cid] <- Array.append [|(r,Some c)|] _array.[cid]
                }
            )
    let agent_Entities =
        let mutable _map = Map.empty<uint32,(uint32*int[] option)[]>
        MailboxProcessor<agent_EntitiesMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | CreateEntity (round,eid,compIDs) ->
                            //match _map.ContainsKey eid with
                            //| true -> ()
                            //| false ->
                            _map <- _map.Add(eid,[|(round,Some compIDs)|])
                        //| DeleteEntity (round,eid) -> 
                        //    if (_map.ContainsKey eid) then 
                        //        _map <- 
                        //            let array = _map.Item eid
                        //            match (snd array.[0]) with
                        //            | None -> _map
                        //            | Some _ -> 
                        //                let newArray = Array.append [|(round,None)|] array
                        //                _map.Remove(eid).Add(eid,newArray)
                        | GetComponents (round,eid,replyChannel) ->
                            replyChannel.Reply(searchArrayDataForRound round (_map.Item eid))
                        //| UpdateComponents (r,eid,compIDs) ->
                        //    match (snd _array.[eid].[0]).IsNone with
                        //    | true -> () //Should probably raise an error
                        //    | false ->
                        //        _array.[eid] <- Array.append [|(r,Some compIDs)|] _array.[eid]
                }
            )
    let agent_EntityID =
        let mutable _maxEntityID = 0u
        MailboxProcessor<agent_EntityIDMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetID replyChannel -> 
                            replyChannel.Reply(_maxEntityID)
                        | InitID startMax -> 
                            _maxEntityID <- startMax
                        | NewID replyChannel -> 
                            _maxEntityID <- _maxEntityID + 1u
                            replyChannel.Reply(_maxEntityID)
                }
            )
    let agent_Locations =
        let mutable _map = Map.empty<LocationDataInt,(uint32*uint32[] option)[]> //  MapLocations |> Array.fold (fun (m:Map<LocationDataInt,(uint32*int[] option)[]>) l -> m.Add(l,Array.empty)) Map.empty
        MailboxProcessor<agent_LocationsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntityToLocation (location,round,eid) -> 
                            _map <-
                                match _map.ContainsKey(location) with
                                | false -> _map.Add(location,[|(round,Some [|eid|])|])
                                | true -> 
                                    let array = _map.Item(location)
                                    let newArray = 
                                        let temp =
                                            match (snd array.[0]) with
                                            | None -> [|(round,Some [|eid|])|]
                                            | Some eids -> [|(round, Some (Array.append eids [|eid|]))|]
                                        Array.append temp array
                                    _map.Remove(location).Add(location,newArray)
                        | GetEntitiesAtLocation (round,location,replyChannel) -> 
                            replyChannel.Reply(
                                match searchArrayDataForRound round (_map.Item(location)) with
                                | None -> [||]
                                | Some eids -> eids
                                )
                        //| GetLocationMap replyChannel -> 
                        //    replyChannel.Reply(_map)
                        | UpdateForm (round,eid,oldForm,newForm) -> 
                            _map <- Map_RemoveValueFromArray _map oldForm.Location (round,Some [|eid|])
                            _map <- Map_AppendValueToArrayUnique _map newForm.Location (round,Some [|eid|])
                        //| RemoveForm fd ->
                        //    _map <- Map_RemoveValueFromArray _map fd.Location fd.EntityID
                }
            )
    //let addEntityToComponents (cts:Component[]) = 
    //    cts 
    //    |> Array.Parallel.iter (fun ct -> agentComponents.Post (AddComponent ct))
    //let addFormToLocations (fd:FormComponent) = agentLocations.Post (AddForm fd)
    let addEntityToLocations (round:uint32) (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        |> Array.Parallel.iter (fun ct -> agent_Locations.Post (AddEntityToLocation (ct.ToForm.Location,round,cts.[0].EntityID)))
    //let removeEntityFromComponents (cts:Component[]) = 
    //    cts 
    //    |> Array.Parallel.iter (fun ct -> agentComponents.Post (RemoveComponent ct))
    //let removeEntityFromLocations (cts:Component[]) =
    //    cts
    //    |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
    //    |> Array.Parallel.iter (fun f -> agentLocations.Post (RemoveForm f.ToForm))

    member _.CreateEntity (round:uint32) (cts:Component[]) = 
        let compIDs = // First store components and get back their IDs
            cts
            |> Array.Parallel.map (fun c -> agent_Components.PostAndReply (fun replyChannel -> CreateComponent (round,c,replyChannel)))

        agent_Entities.Post (CreateEntity (round,cts.[0].EntityID,compIDs))
        //addEntityToComponents cts
        //addEntityToLocations cts
        addEntityToLocations round cts
        Ok None

    member me.EntityExists (round:uint32 option) (entityID:uint32) = //agentEntities.PostAndReply (fun replyChannel -> EntityExists(entityID,replyChannel))
        match me.GetComponents round entityID with
        | [||] -> false
        | _ -> true
    //member _.GetAllHistory() = agentHistory.PostAndReply GetAllHistory
    //member _.GetComponent (componentID:byte) (entityID:uint32) = 
    //    agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
    //    |> Array.find (fun c -> c.ComponentID = componentID)
    //member _.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
    member _.GetComponents (round:uint32 option) (entityID:uint32) : Component[] = 
        let compIDso = agent_Entities.PostAndReply (fun replyChannel -> GetComponents(round,entityID,replyChannel))
        match compIDso with
        | None -> [||]
        | Some compIDs -> 
            compIDs |> Array.Parallel.choose (fun cid -> agent_Components.PostAndReply (fun replyChannel -> GetComponent(round,cid,replyChannel)))
    member _.GetEntitiesAtLocation (round:uint32 option) (location:LocationDataInt) = 
        agent_Locations.PostAndReply (fun replyChannel -> GetEntitiesAtLocation (round,location,replyChannel))
    //member _.GetEntitiesWithComponent (componentID:byte) = agentComponents.PostAndReply (fun replyChannel -> GetEntitiesWithComponent (componentID,replyChannel))
    //member _.GetEntityMap() = agentEntities.PostAndReply GetEntityMap
    //member me.GetHistory (round:uint32 option) = 
    //    match round with
    //    | None -> (me.GetEntityMap(), me.GetComponentMap(), me.GetLocationMap())
    //    | Some r when r = 0u -> (me.GetEntityMap(), me.GetComponentMap(), me.GetLocationMap())
    //    | _ -> agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
    //    //agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
    //member _.GetLocationMap() = agentLocations.PostAndReply GetLocationMap
    member _.GetMaxID = agent_EntityID.PostAndReply GetID
    member _.GetNewID = agent_EntityID.PostAndReply NewID
    //member _.Init (history:Map<uint32,historyTuple>) (startMax:uint32) round = 
    //    let map,_,_ = history.Item(round)
    //    let ctss = map |> MapValuesToArray
    //    Async.Parallel 
    //    (
    //        agentEntities.Post (agent_EntitiesMsg.InitEntities map)
    //        agentID.Post (agent_IDMsg.InitID startMax)
    //        ctss |> Array.Parallel.iter addEntityToComponents
    //        ctss |> Array.Parallel.iter addEntityToLocations
    //        agentHistory.Post (InitHistory history)
    //    ) |> ignore
    //member _.PendingUpdates = 
    //    agentEntities.CurrentQueueLength > 0 || 
    //    agent_EntityID.CurrentQueueLength > 0 || 
    //    agentComponents.CurrentQueueLength > 0 || 
    //    agentLocations.CurrentQueueLength > 0
    //member me.RecordHistory round = agentHistory.Post (RecordHistory (round,(me.GetEntityMap(),me.GetComponentMap(),me.GetLocationMap())))
    //member me.RemoveEntity (entityID:uint32) = 
    //    let cts = me.GetComponents entityID
    //    Async.Parallel 
    //    (
    //        agentEntities.Post (RemoveEntity entityID)
    //        removeEntityFromComponents cts
    //        removeEntityFromLocations cts
    //    ) |> ignore
    //    Ok None
    member me.RemoveComponent (round:uint32) (compID:int) =
        ()

    member me.UpdateComponent (round:uint32) (compID:int) (comp:Component) = 
        //match c with
        //| Form f -> agentLocations.Post (MoveForm ((me.GetComponent FormComponentID c.EntityID).ToForm,f))
        //| _ -> ()
        //agentEntities.Post (ReplaceComponent c)
        agent_Components.Post (UpdateComponent (round,compID,comp))

