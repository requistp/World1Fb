module Entities
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent
open LocationTypes

type historyTuple = Map<uint32,Component[]> * Map<byte,uint32[]> * Map<LocationDataInt,uint32[]>

type private agent_HistoryMsg = 
    | GetHistory of round:uint32 option * history:AsyncReplyChannel<historyTuple>
    | GetAllHistory of allHistory:AsyncReplyChannel<Map<uint32,historyTuple> >
    | InitHistory of allHistory:Map<uint32,historyTuple>
    | RecordHistory of round:uint32 * history:historyTuple

type private agent_ComponentsMsg = 
    | AddComponent of Component
    | GetEntitiesWithComponent of componentID:byte * entityID:AsyncReplyChannel<uint32[]>
    | GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
    | RemoveComponent of Component

type private agent_EntitiesMsg = 
    | AddEntity of Component[]
    | EntityExists of entityID:uint32 * AsyncReplyChannel<bool>
    | GetComponents of entityID:uint32 * AsyncReplyChannel<Component[]>
    | GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
    | InitEntities of Map<uint32,Component[]>
    | RemoveEntity of entityID:uint32
    | ReplaceComponent of Component

type private agent_IDMsg = 
    | GetID of maxEntityID:AsyncReplyChannel<uint32>
    | InitID of maxEntityID:uint32
    | NewID of AsyncReplyChannel<uint32>

type private agent_LocationsMsg =
    | AddForm of FormComponent
    | GetEntitiesAtLocation of LocationDataInt * AsyncReplyChannel<uint32[]>
    | GetLocationMap of AsyncReplyChannel<Map<LocationDataInt,uint32[]> >
    | MoveForm of oldForm:FormComponent * newForm:FormComponent
    | RemoveForm of FormComponent

type Entities() = 
    let agentComponents =
        let mutable _map = Map.empty<byte,uint32[]>
        MailboxProcessor<agent_ComponentsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddComponent c ->
                            match _map.ContainsKey c.ComponentID with
                            | false -> _map <- _map.Add(c.ComponentID,[|c.EntityID|])
                            | true -> 
                                _map <- Map_AppendValueToArrayUnique _map c.ComponentID c.EntityID 
                        | GetEntitiesWithComponent (cid,replyChannel) -> 
                            replyChannel.Reply(
                                match _map.ContainsKey(cid) with
                                | false -> Array.empty
                                | true -> _map.Item(cid))
                        | GetComponentMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | RemoveComponent ct ->
                            _map <- Map_RemoveValueFromArray _map ct.ComponentID ct.EntityID
                }
            )
    let agentEntities =
        let mutable _ecmap = Map.empty<uint32,Component[]>
        MailboxProcessor<agent_EntitiesMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntity cts ->
                            if not (_ecmap.ContainsKey(cts.[0].EntityID)) then 
                                _ecmap <- _ecmap.Add(cts.[0].EntityID,cts)
                        | EntityExists (eid,replyChannel) ->
                            replyChannel.Reply(_ecmap.ContainsKey eid)
                        | GetComponents (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _ecmap.ContainsKey eid with
                                | false -> [||]
                                | true -> _ecmap.Item(eid))
                        | GetEntityMap replyChannel ->
                            replyChannel.Reply(_ecmap)
                        | InitEntities map -> 
                            _ecmap <- map
                        | RemoveEntity eid -> 
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
    let agentID =
        let mutable _maxEntityID = 0u
        MailboxProcessor<agent_IDMsg>.Start(
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
    let agentHistory =
        let mutable _history = Map.empty<uint32,historyTuple>
        MailboxProcessor<agent_HistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetHistory (round,replyChannel) ->
                            replyChannel.Reply(
                                match round with
                                | Some r -> _history.Item r
                                | None -> _history.Item (uint32 (_history.Count - 1))
                                )
                        | GetAllHistory replyChannel ->
                            replyChannel.Reply(_history)
                        | InitHistory hs ->
                            _history <- hs
                        | RecordHistory (round,h) -> 
                            _history <- _history.Add(round,h)
                }
            )
    let agentLocations =
        let mutable _map = MapLocations |> Array.fold (fun (m:Map<LocationDataInt,uint32[]>) l -> m.Add(l,Array.empty)) Map.empty
        MailboxProcessor<agent_LocationsMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddForm fd ->
                            _map <- Map_AppendValueToArrayUnique _map fd.Location fd.EntityID 
                        | GetEntitiesAtLocation (location,replyChannel) -> 
                            replyChannel.Reply(_map.Item(location))
                        | GetLocationMap replyChannel -> 
                            replyChannel.Reply(_map)
                        | MoveForm (oldForm,newForm) -> 
                            _map <- Map_RemoveValueFromArray _map oldForm.Location oldForm.EntityID
                            _map <- Map_AppendValueToArrayUnique _map newForm.Location newForm.EntityID 
                        | RemoveForm fd ->
                            _map <- Map_RemoveValueFromArray _map fd.Location fd.EntityID
                }
            )

    let addEntityToComponents (cts:Component[]) = 
        cts 
        |> Array.Parallel.iter (fun ct -> agentComponents.Post (AddComponent ct))
    let addFormToLocations (fd:FormComponent) = agentLocations.Post (AddForm fd)
    let addEntityToLocations (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        |> Array.Parallel.iter (fun ct -> addFormToLocations ct.ToForm)
    let removeEntityFromComponents (cts:Component[]) = 
        cts 
        |> Array.Parallel.iter (fun ct -> agentComponents.Post (RemoveComponent ct))
    let removeEntityFromLocations (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        |> Array.Parallel.iter (fun f -> agentLocations.Post (RemoveForm f.ToForm))

    member _.CreateEntity (cts:Component[]) = 
        Async.Parallel 
        (
            agentEntities.Post (AddEntity cts)
            addEntityToComponents cts
            addEntityToLocations cts
        ) |> ignore
        Ok None
    member _.EntityExists (entityID:uint32) = agentEntities.PostAndReply (fun replyChannel -> EntityExists(entityID,replyChannel))
    member _.GetAllHistory() = agentHistory.PostAndReply GetAllHistory
    member _.GetComponent (componentID:byte) (entityID:uint32) = 
        agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
        |> Array.find (fun c -> c.ComponentID = componentID)
    member _.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
    member _.GetComponents (entityID:uint32) = agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
    member _.GetEntitiesAtLocation (location:LocationDataInt) = agentLocations.PostAndReply (fun replyChannel -> GetEntitiesAtLocation (location,replyChannel))
    member _.GetEntitiesWithComponent (componentID:byte) = agentComponents.PostAndReply (fun replyChannel -> GetEntitiesWithComponent (componentID,replyChannel))
    member _.GetEntityMap() = agentEntities.PostAndReply GetEntityMap
    member _.GetHistory (round:uint32 option) = agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
    member _.GetLocationMap() = agentLocations.PostAndReply GetLocationMap
    member _.GetMaxID = agentID.PostAndReply GetID
    member _.GetNewID = agentID.PostAndReply NewID
    member _.Init (map:Map<uint32,Component[]>) (startMax:uint32) = 
        let ctss = map |> MapValuesToArray
        Async.Parallel 
        (
            agentEntities.Post (agent_EntitiesMsg.InitEntities map)
            agentID.Post (agent_IDMsg.InitID startMax)
            ctss |> Array.Parallel.iter (fun cts -> addEntityToComponents cts)
            ctss |> Array.Parallel.iter (fun cts -> addEntityToLocations cts)
        ) |> ignore
    //member _.InitHistory allHistory = agentHistory.Post (InitHistory allHistory)
    member _.PendingUpdates = 
        agentEntities.CurrentQueueLength > 0 || 
        agentID.CurrentQueueLength > 0 || 
        agentComponents.CurrentQueueLength > 0 || 
        agentLocations.CurrentQueueLength > 0
    member _.RecordHistory round history = agentHistory.Post (RecordHistory (round,history))
    member me.RemoveEntity (entityID:uint32) = 
        let cts = me.GetComponents entityID
        Async.Parallel 
        (
            agentEntities.Post (RemoveEntity entityID)
            removeEntityFromComponents cts
            removeEntityFromLocations cts
        ) |> ignore
        Ok None
    member me.ReplaceComponent (c:Component) = 
        match c with
        | Form f -> agentLocations.Post (MoveForm ((me.GetComponent FormComponentID c.EntityID).ToForm,f))
        | _ -> ()
        agentEntities.Post (ReplaceComponent c)


