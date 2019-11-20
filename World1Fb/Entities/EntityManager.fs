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
    let agent_Entities = new agent_EntityManager()
    let agent_Locations = new agent_Locations()
    let agent_ComponentTypes = new agent_ComponentTypes()


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

    member _.CreateEntity (round:RoundNumber) (cts:Component[]) = 
        cts
        |> Array.Parallel.iter (fun c -> agent_Components.Add round c)

        cts
        |> Array.Parallel.map (fun c -> c.ID)
        |> agent_Entities.Add round (cts.[0].EntityID)

        cts |> Array.Parallel.iter (fun c -> agent_ComponentTypes.Add round c.ComponentTypeID c.ID)

        cts |> Array.Parallel.iter (fun c -> if c.ComponentTypeID = FormComponentID then agent_Locations.Add round (ToForm c))
        Ok None

    member me.EntityExists (round:uint32 option) (entityID:uint32) = //agentEntities.PostAndReply (fun replyChannel -> EntityExists(entityID,replyChannel))
        match me.GetComponents round entityID with
        | [||] -> false
        | _ -> true
    //member _.GetAllHistory() = agentHistory.PostAndReply GetAllHistory
    //member _.GetComponent (cid:ComponentID) (eid:EntityID) = 
    //    agentEntities.PostAndReply (fun replyChannel -> GetComponents(entityID,replyChannel))
    //    |> Array.find (fun c -> c.ComponentID = componentID)
    //member _.GetComponentMap() = agentComponents.PostAndReply GetComponentMap
    member me.GetComponentByType (round:RoundNumber option) (ctid:ComponentTypeID) (eid:EntityID) = 
        eid
        |> me.GetComponents round
        |> Array.filter (fun (c:Component) -> c.ComponentTypeID = ctid)
    member _.GetComponentIDs (round:RoundNumber option) (eid:EntityID) = 
        match (agent_Entities.Get round eid) with
        | None -> [||]
        | Some cids -> cids
    member me.GetComponents (round:RoundNumber option) (eid:EntityID) = 
        eid
        |> me.GetComponentIDs round
        |> Array.Parallel.choose (fun cid -> agent_Components.Get round cid)
    member _.GetComponents_ByID round cids = cids |> Array.Parallel.choose (agent_Components.Get round)
    member _.GetFormsAtLocation (round:RoundNumber option) (location:LocationDataInt) = 
        match (agent_Locations.Get round location) with
        | None -> Array.empty
        | Some fcids -> fcids |> Array.Parallel.choose (agent_Components.Get round)
    member me.GetEntityIDsAtLocation (round:RoundNumber option) (location:LocationDataInt) = 
        me.GetFormsAtLocation round location
        |> Array.Parallel.map (fun f -> f.EntityID)
    //member _.GetEntitiesAtLocation (round:RoundNumber option) (location:LocationDataInt) = 
    //    match (agent_Locations.Get round location) with
    //    | None -> [||]
    //    | Some eids -> eids
    member _.GetComponentIDsByType round ctid = 
        match (agent_ComponentTypes.Get round ctid) with
        | None -> Array.empty
        | Some cids -> cids

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
    member me.RemoveEntity (round:RoundNumber) (eid:EntityID) = 
        let cts = eid |> me.GetComponents (Some round)
        
        Async.Parallel
        (
            cts |> Array.Parallel.iter (agent_Components.Remove round)

            cts |> Array.Parallel.iter (agent_ComponentTypes.Remove round)

            cts |> Array.Parallel.iter (fun c -> if c.ComponentTypeID = FormComponentID then agent_Locations.Remove round (ToForm c))

            agent_Entities.Remove round eid
        ) |> ignore
        Ok None
    member _.NewComponentID() = agent_Components.NewComponentID()
    member _.NewEntityID() = agent_Entities.NewEntityID()
    //member me.RemoveComponent (round:uint32) (compID:int) =
    //    ()

    member me.UpdateComponent round comp = 
        match comp with
        | Form f -> 
            let oldForm = (me.GetComponentByType None FormComponentID f.EntityID).[0].ToForm
            match oldForm.Location = f.Location with
            | true -> ()
            | false -> agent_Locations.Move round oldForm f
        | _ -> ()
        agent_Components.Update round comp




