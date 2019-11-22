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
    let agent_Entities = new agent_EntityManager(agent_Components)
    let agent_Locations = new agent_Locations(agent_Components)
    let agent_ComponentTypes = new agent_ComponentTypes(agent_Components)
    
    member  _.CreateEntity (round:RoundNumber) (cts:Component[]) = 
        Async.Parallel 
        (
            agent_Components.AddMany round cts

            agent_ComponentTypes.AddMany round cts

            cts |> Array.iter (fun c -> if c.ComponentTypeID = FormComponentID then agent_Locations.Add round (ToForm c))

            agent_Entities.Add round cts       
        ) 
        Ok None
    member  _.CreateEntityMany (round:RoundNumber) (ctss:Component[][]) = 
        Async.Parallel 
        (
            ctss |> Array.iter (agent_Components.AddMany round)

            ctss |> Array.iter (agent_ComponentTypes.AddMany round)

            ctss |> Array.iter (fun cts -> cts |> Array.iter (fun c -> if c.ComponentTypeID = FormComponentID then agent_Locations.Add round (ToForm c)))

            ctss |> Array.iter (agent_Entities.Add round)
        ) 
        Ok None
    member me.EntityExists round entityID = 
        match me.GetComponents round entityID with
        | [||] -> false
        | _ -> true
    member me.GetComponent round ctid eid = 
        eid
        |> me.GetComponents round
        |> Array.find (fun (c:Component) -> c.ComponentTypeID = ctid)
    member  _.GetComponents round eid = agent_Entities.Get round eid
    member  _.GetComponentsOfType round ctid = agent_ComponentTypes.Get round ctid
    member  _.GetFormsAtLocation round location = agent_Locations.Get round location
    member me.GetEntityIDsAtLocation round location = 
        me.GetFormsAtLocation round location
        |> Array.Parallel.map (fun f -> f.EntityID)
    member  _.GetLocationMap round = agent_Locations.GetMap round
    member  _.NewComponentID() = agent_Components.NewComponentID()
    member  _.NewEntityID() = agent_Entities.NewEntityID()
    member me.RemoveEntity round eid = 
        let cts = eid |> me.GetComponents (Some round)
        
        Async.Parallel
        (
            agent_Components.RemoveMany round cts

            agent_ComponentTypes.RemoveMany round cts

            cts |> Array.iter (fun c -> if c.ComponentTypeID = FormComponentID then agent_Locations.Remove round (ToForm c))

            agent_Entities.Remove round eid
        )
        Ok None
    member me.UpdateComponent round comp = 
        match comp with
        | Form f -> 
            let oldForm = ToForm (f.EntityID |> me.GetComponent None FormComponentID)
            if (oldForm.Location <> f.Location) then
                agent_Locations.Move round oldForm f
        | _ -> ()
        
        agent_Components.Update round comp
        



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

