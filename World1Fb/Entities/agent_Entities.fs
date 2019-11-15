module agent_Entities
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent
open LocationTypes

type private agent_ComponentsMsg = 
    | AddComponent of Component
    | GetEntitiesWithComponent of componentID:byte * entityID:AsyncReplyChannel<uint32[]>
    | GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
    | InitComponents of Map<byte,uint32[]>    
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
    | InitLocations of Map<LocationDataInt,uint32[]>
    | MoveForm of oldForm:FormComponent * newForm:FormComponent
    | RemoveForm of FormComponent

type agent_EntityComponent() = 
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
                        | InitComponents newMap -> 
                            _map <- newMap
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
                        | InitLocations newMap -> 
                            _map <- newMap
                        | MoveForm (oldForm,newForm) -> 
                            _map <- Map_RemoveValueFromArray _map oldForm.Location oldForm.EntityID
                            _map <- Map_AppendValueToArrayUnique _map newForm.Location newForm.EntityID 
                        | RemoveForm fd ->
                            _map <- Map_RemoveValueFromArray _map fd.Location fd.EntityID
                }
            )
    member me.CreateEntity (cts:Component[]) = 
        Async.Parallel 
        (
            agentEntities.Post (AddEntity cts)
            me.AddComponentsToMap cts
            me.AddForm cts
        ) |> ignore
        Ok None
    member _.EntityExists (entityID:uint32) = agentEntities.PostAndReply (fun replyChannel -> EntityExists(entityID,replyChannel))
    member _.GetComponents (entityID:uint32) = agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
    member me.GetComponent (componentID:byte) (entityID:uint32) = me.GetComponents entityID |> Array.find (fun c -> c.ComponentID = componentID)
    member _.GetEntities() = agentEntities.PostAndReply GetEntityMap
    member _.GetMaxID = agentID.PostAndReply GetID
    member _.GetNewID = agentID.PostAndReply NewID
    member me.Init (map:Map<uint32,Component[]>) (startMax:uint32) = 
        let ctss = map |> MapValuesToArray
        Async.Parallel 
        (
            agentEntities.Post (agent_EntitiesMsg.InitEntities map)
            agentID.Post (agent_IDMsg.InitID startMax)
            ctss |> Array.Parallel.iter (fun cts -> me.AddComponentsToMap cts)
            ctss |> Array.Parallel.iter (fun cts -> cts |> Array.filter (fun c -> c.ComponentID=FormComponentID) |> Array.Parallel.iter (fun c -> me.AddForm c.ToForm))
        ) |> ignore
    member _.PendingUpdates = agentEntities.CurrentQueueLength > 0 || agentID.CurrentQueueLength > 0 || agentComponents.CurrentQueueLength > 0 || agentLocations.CurrentQueueLength > 0
    member me.RemoveEntity (entityID:uint32) = 
        let cts = me.GetComponents entityID
        Async.Parallel 
        (
            agentEntities.Post (RemoveEntity entityID)
            me.RemoveComponentsFromMap cts
            me.RemoveForm cts
        ) |> ignore
        Ok None
    member me.ReplaceComponent (c:Component) = 
        match c with
        | Form f -> f |> me.MoveForm (me.GetComponent FormComponentID c.EntityID).ToForm
        | _ -> ()
        agentEntities.Post (ReplaceComponent c)

    member private me.AddComponentsToMap (cts:Component[]) = cts |> Array.Parallel.iter (fun ct -> agentComponents.Post (AddComponent ct))
    member me.GetEntitiesWithComponent (componentID:byte) = agentComponents.PostAndReply (fun replyChannel -> GetEntitiesWithComponent (componentID,replyChannel))
    member me.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
    member private me.RemoveComponentsFromMap (cts:Component[]) = cts |> Array.Parallel.iter (fun ct -> agentComponents.Post (RemoveComponent ct))

    member _.AddForm (fd:FormComponent) = agentLocations.Post (AddForm fd)
    member me.AddForm (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        |> Array.Parallel.iter (fun ct -> me.AddForm ct.ToForm)
    member _.GetEntitiesAtLocation (location:LocationDataInt) = agentLocations.PostAndReply (fun replyChannel -> GetEntitiesAtLocation (location,replyChannel))
    member _.GetLocationMap() = agentLocations.PostAndReply GetLocationMap
    member _.MoveForm (oldForm:FormComponent) (newForm:FormComponent) = agentLocations.Post (MoveForm (oldForm,newForm))
    member _.RemoveForm (fd:FormComponent) = agentLocations.Post (RemoveForm fd)
    member me.RemoveForm (cts:Component[]) =
        cts
        |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
        |> Array.Parallel.iter (fun ct -> me.RemoveForm ct.ToForm)


