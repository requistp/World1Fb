module EntityManager
open agent_Entities
open agent_EntityHistory
open agent_GameLog
open Component
open ComponentEnums
open CommonGenericFunctions
open LocationTypes


type EntityManager(log:agent_GameLog) =
    let agentForEntities = new agent_EntityComponent() 
    let agentForHistory = new agent_EntityHistory()

    member _.AgentHistory = agentForHistory
    member _.AgentEntities = agentForEntities

    member me.GetComponent (cid:byte) (eid:uint32) =
        eid
        |> agentForEntities.GetComponents 
        |> Array.find (fun x -> x.ComponentID = cid)

    member me.ReplaceComponent (c:Component) =
        agentForEntities.ReplaceComponent c

    member _.TryGet (eid:uint32) =
        agentForEntities.EntityExists eid |> TrueSomeFalseNone (agentForEntities.GetComponents eid)

    member me.TryGetComponent (componentID:byte) (entityID:uint32) : Option<Component> = 
        entityID
        |> me.TryGet 
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentID = componentID) with
            | [||] -> None
            | l -> Some (l.[0]))


module rec History = 

    let CopyEntity (agentEntity:agent_EntityComponent) (oldeid:uint32) =
        let neweid = agentEntity.GetNewID
        oldeid
        |> agentEntity.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    //let CreateEntity (agentEntity:agent_EntityComponent) (components:Component[]) = 
    //    agentEntity.CreateEntity components
    //    Ok None

    let GetComponent2 (entities:Map<uint32,Component[]>) (componentID:byte) (entityID:uint32) =
        entities.Item(entityID)
        |> Array.find (fun x -> x.ComponentID = componentID)

    let GetComponent (agentEntity:agent_EntityComponent) (componentID:byte) (entityID:uint32) =
        entityID
        |> agentEntity.GetComponents
        |> Array.find (fun x -> x.ComponentID = componentID)

    let GetComponentForEntities (agentEntity:agent_EntityComponent) (componentID:byte) (entityIDs:uint32[]) = 
        componentID
        |> agentEntity.GetEntitiesWithComponent
        |> Array.filter (fun e -> entityIDs |> Array.contains e)
        |> Array.Parallel.map (fun e -> GetComponent agentEntity componentID e)

    let GetComponentIDs (agentEntity:agent_EntityComponent) (eid:uint32) =
        eid 
        |> agentEntity.GetComponents 
        |> Array.Parallel.map (fun ct -> ct.ComponentID)

    let GetEntities (agentHistory:agent_EntityHistory) (round:uint32 option) = 
        let e,_,_ = agentHistory.GetHistory round
        e

    let GetEntitiesAtLocation (agentHistory:agent_EntityHistory) (round:uint32 option) (location:LocationDataInt) =
        let _,_,locations = agentHistory.GetHistory round
        match locations.ContainsKey location with
        | false -> [||]
        | true -> locations.Item location

    let GetEntitiesAtLocationWithComponent (agentEntity:agent_EntityComponent) (componentID:byte) (excludeEntityID:uint32 option) (location:LocationDataInt) = 
        location
        |> agentEntity.GetEntitiesAtLocation
        |> Array.filter (fun eid -> excludeEntityID.IsNone || eid <> excludeEntityID.Value) // Not excluded or not me
        |> Array.Parallel.choose (fun eid -> eid |> TryGetComponent agentEntity componentID)

    let GetEntitiesWithComponent (agentHistory:agent_EntityHistory) (round:uint32 option) (componentID:byte) = 
        let _,components,_ = agentHistory.GetHistory round
        match components.ContainsKey componentID with
        | false -> [||]
        | true -> components.Item componentID
    
    let GetLocation (agentEntity:agent_EntityComponent) (entityID:uint32) = (entityID|>GetComponent agentEntity FormComponentID).ToForm.Location

    let Init (agentEntity:agent_EntityComponent) (map:Map<uint32,Component[]>) (maxEntityID:uint32) =
        agentEntity.Init map maxEntityID
        //////let ctss = map |> MapValuesToArray
        //////ctss |> Array.Parallel.iter (fun cts -> agentComponents.AddComponentsToMap cts)
        //////ctss |> Array.Parallel.iter (fun cts -> cts |> Array.filter (fun c -> c.ComponentID=FormComponentID) |> Array.Parallel.iter (fun c -> agentLocations.Add c.ToForm))

    //let PendingUpdates (agentEntity:agent_EntityComponent) (agentComponents:agent_ComponentEntity) (agentLocations:agent_LocationEntity) (agentHistory:agent_EntityHistory) = 
    //    agentEntity.PendingUpdates || agentComponents.PendingUpdates || agentLocations.PendingUpdates || agentHistory.PendingUpdates

    //let RemoveEntity (agentEntity:agent_EntityComponent) (agentLocations:agent_LocationEntity) (eid:uint32) =
    //    agentEntity.RemoveEntity eid

    let TryGet (agentEntity:agent_EntityComponent) (entityID:uint32) =
        entityID 
        |> agentEntity.EntityExists 
        |> TrueSomeFalseNone (agentEntity.GetComponents entityID)

    let TryGetComponent (agentEntity:agent_EntityComponent) (componentID:byte) (entityID:uint32) : Option<Component> = 
        entityID
        |> TryGet agentEntity
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentID = componentID) with
            | [||] -> None
            | l -> Some (l.[0]))



