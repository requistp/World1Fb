module EntityManager
open agent_Components
open agent_ComponentTypes
open agent_EntityManager
open agent_Locations
open CommonGenericFunctions
open Component
open ComponentEnums
open FormComponent
open LocationTypes


type EntityManager() = 
    let agent_Components = new agent_Components()
    let agent_ComponentTypes = new agent_ComponentTypes(agent_Components)
    let agent_Entities = new agent_EntityManager(agent_Components)
    let agent_Locations = new agent_Locations(agent_Components)
    
    member  _.CreateEntity (cts:Component[]) = 
        Async.Parallel 
        (
            agent_Components.AddMany cts

            agent_ComponentTypes.AddMany cts

            cts |> Array.iter (fun c -> if GetComponentType c = FormComponent then agent_Locations.Add (ToForm c))

            agent_Entities.Add cts       
        ) 
        Ok None
    member  _.CreateEntityMany (ctss:Component[][]) = 
        Async.Parallel 
        (
            ctss |> Array.iter agent_Components.AddMany

            ctss |> Array.iter agent_ComponentTypes.AddMany

            ctss |> Array.iter (fun cts -> cts |> Array.iter (fun c -> if GetComponentType c = FormComponent then agent_Locations.Add (ToForm c)))

            ctss |> Array.iter agent_Entities.Add
        ) 
        Ok None
    member me.EntityExists entityID = 
        match me.GetComponents entityID with
        | [||] -> false
        | _ -> true
    member me.GetComponent ctid eid = 
        eid
        |> me.GetComponents
        |> Array.find (fun (c:Component) -> GetComponentType c = ctid)
    member  _.GetComponents eid = agent_Entities.Get eid
    member  _.GetComponentsOfType ctid = agent_ComponentTypes.Get ctid
    member  _.GetFormsAtLocation location = agent_Locations.Get location
    member  _.GetForSave_Components = agent_Components.GetMap
    member  _.GetForSave_ComponentTypes = agent_ComponentTypes.GetMap
    member  _.GetForSave_Entities = agent_Entities.GetMap
    member  _.GetForSave_Locations = agent_Locations.GetMap
    member me.GetEntityIDsAtLocation location = me.GetFormsAtLocation location |> Array.Parallel.map (fun f -> f.EntityID)
    member  _.GetLocationMap = agent_Locations.GetMap_Forms 
    member  _.Init (compMap:Map<ComponentID,Component>) (compTypeMap:Map<ComponentType,ComponentID[]>) (entityMap:Map<EntityID,ComponentID[]>) (locationMap:Map<LocationDataInt,ComponentID[]>) =
        agent_Components.Init compMap
        agent_ComponentTypes.Init compTypeMap
        agent_Entities.Init entityMap
        agent_Locations.Init locationMap
    member  _.NewComponentID() = agent_Components.NewComponentID()
    member  _.NewEntityID() = agent_Entities.NewEntityID()
    member me.RemoveEntity eid = 
        let cts = me.GetComponents eid
        
        Async.Parallel
        (
            agent_Components.RemoveMany cts

            agent_ComponentTypes.RemoveMany cts

            cts |> Array.iter (fun c -> if GetComponentType c = FormComponent then agent_Locations.Remove (ToForm c))

            agent_Entities.Remove eid
        )
        Ok None
    member me.UpdateComponent round comp = 
        match comp with
        | Form f -> 
            let oldForm = ToForm (me.GetComponent FormComponent f.EntityID)
            if (oldForm.Location <> f.Location) then
                agent_Locations.Move oldForm f
        | _ -> ()
        
        agent_Components.Update comp
        



//let addEntityToComponents (cts:Component[]) = 
//    cts 
//    |> Array.Parallel.iter (fun ct -> agentComponents.Post (AddComponent ct))
//let addFormToLocations (fd:FormComponent) = agentLocations.Post (AddForm fd)
//let addEntityToLocations (round:uint32) (cts:Component[]) =
//    cts
//    |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
//    |> Array.Parallel.iter (fun ct -> agent_Locations.Post (AddEntityToLocation (ct.ToForm.Location,round,cts.[0].EntityID)))
//let removeEntityFromComponents (cts:Component[]) = 
//    cts 
//    |> Array.Parallel.iter (fun ct -> agentComponents.Post (RemoveComponent ct))
//let removeEntityFromLocations (cts:Component[]) =
//    cts
//    |> Array.filter (fun ct -> ct.ComponentID = FormComponentID)
//    |> Array.Parallel.iter (fun f -> agentLocations.Post (RemoveForm f.ToForm))
//member _.GetAllHistory() = agentHistory.PostAndReply GetAllHistory
//member _.GetComponent (cid:ComponentID) (eid:EntityID) = 
//    agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
//    |> Array.find (fun c -> c.ComponentID = componentID)
//member _.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
//member _.GetComponentIDs (round:RoundNumber option) (eid:EntityID) = 
//    match (agent_Entities.Get round eid) with
//    | None -> [||]
//    | Some cids -> cids
//member _.GetComponents_ByID round cids = cids |> Array.Parallel.choose (agent_Components.Get round)
//member _.GetEntitiesAtLocation (round:RoundNumber option) (location:LocationDataInt) = 
//    match (agent_Locations.Get round location) with
//    | None -> [||]
//    | Some eids -> eids
//member _.GetComponentIDsByType round ctid = 
//    match (agent_ComponentTypes.Get round ctid) with
//    | None -> Array.empty
//    | Some cids -> cids

//member _.GetEntityMap() = agentEntities.PostAndReply GetEntityMap
//member me.GetHistory (round:uint32 option) = 
//    match round with
//    | None -> (me.GetEntityMap(), me.GetComponentMap(), me.GetLocationMap())
//    | Some r when r = 0u -> (me.GetEntityMap(), me.GetComponentMap(), me.GetLocationMap())
//    | _ -> agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
//    //agentHistory.PostAndReply (fun replyChannel -> GetHistory (round,replyChannel))
//member _.GetLocationMap() = agentLocations.PostAndReply GetLocationMap
//member _.GetMaxID = agent_EntityID.PostAndReply GetID
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

