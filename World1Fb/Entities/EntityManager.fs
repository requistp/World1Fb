module EntityManager
open agent_ComponentEntity
open agent_EntityComponent
open agent_EntityID
open agent_EntityHistory
open agent_GameLog
open agent_LocationEntity
open Component
open ComponentEnums
open CommonGenericFunctions
open LocationTypes
open System


type EntityManager(log:agent_GameLog) =
    let agentForEntityID = new agent_EntityID()
    let agentForComponents = new agent_ComponentEntity()
    let agentForLocations = new agent_LocationEntity()
    let agentForEntities = new agent_EntityComponent() 
    let agentForHistory = new agent_EntityHistory()

    member _.CopyEntity (oldeid:uint32) =
        let neweid = agentForEntityID.GetNewID
        oldeid
        |> agentForEntities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    member _.CreateEntity (cts:Component[]) = 
        Async.Parallel 
        (
            agentForEntities.CreateEntity cts
            agentForComponents.Add cts
            agentForLocations.Add cts
        ) |> ignore
        Ok None

    member _.Exists eid = agentForEntities.Exists eid

    member _.GetComponents (eid:uint32) = agentForEntities.GetComponents eid

    member me.GetComponent (cid:byte) (eid:uint32) =
        eid
        |> me.GetComponents 
        |> Array.find (fun x -> x.ComponentID = cid)

    member _.GetEntities = agentForEntities.GetAll

    member _.GetEntitiesWithComponent c = agentForComponents.Get c

    member _.GetEntitiesAtLocation location = agentForLocations.Get location

    member me.GetEntitiesAtLocationWithComponent (excludeEntityID:uint32 option) (componentID:byte) (location:LocationDataInt) = 
        location
        |> me.GetEntitiesAtLocation
        |> Array.filter (fun eid -> excludeEntityID.IsNone || eid <> excludeEntityID.Value) // Not excluded or not me
        |> Array.Parallel.choose (fun eid -> eid |> me.TryGetComponent componentID)

    member _.GetHistory (round:uint32 option) = agentForHistory.Get round
    
    member me.GetLocation (entityID:uint32) = (entityID|>me.GetComponent FormComponentID).ToForm.Location

    member _.GetMaxID = agentForEntityID.GetMaxID 

    member _.GetNewID = agentForEntityID.GetNewID

    member _.HasAllComponents (cts:byte[]) (eid:uint32) =
        let ects = agentForEntities.GetComponents eid |> Array.Parallel.map (fun ct -> ct.ComponentID)
        cts |> Array.forall (fun ct -> ects |> Array.contains ct)

    member _.Init (map:Map<uint32,Component[]>) =
        agentForEntities.Init map
        let ctss = map |> MapValuesToArray
        ctss |> Array.Parallel.iter (fun cts -> agentForComponents.Add cts)
        ctss |> Array.Parallel.iter (fun cts -> cts |> Array.filter (fun c -> c.ComponentID=FormComponentID) |> Array.Parallel.iter (fun c -> agentForLocations.Add c.ToForm))
    
    member _.Init (maxEntityID:uint32) = agentForEntityID.Init maxEntityID

    member _.PendingUpdates = 
        agentForEntities.PendingUpdates || agentForEntityID.PendingUpdates || agentForComponents.PendingUpdates || agentForLocations.PendingUpdates || agentForHistory.PendingUpdates

    member me.RecordHistory round =
        agentForHistory.Add round (agentForEntities.GetAll(), agentForComponents.GetAll(), agentForLocations.GetAll())

    member _.RemoveEntity (eid:uint32) =
        let cts = agentForEntities.GetComponents eid
        Async.Parallel 
        (
            agentForComponents.Remove cts
            agentForLocations.Remove cts
            agentForEntities.RemoveEntity eid
        ) |> ignore
        Ok None
        
    member me.ReplaceComponent (c:Component) =
        match c with
        | Form f -> f |> agentForLocations.Move (me.GetComponent FormComponentID c.EntityID).ToForm
        | _ -> ()
        agentForEntities.ReplaceComponent c

    member _.TryGet (eid:uint32) =
        agentForEntities.Exists eid |> TrueSomeFalseNone (agentForEntities.GetComponents eid)

    member me.TryGetComponent (cid:byte) (eid:uint32) : Option<Component> = 
        me.TryGet eid
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentID = cid) with
            | [||] -> None
            | l -> Some (l.[0]))
        //match me.TryGet eid with
        //| None -> None
        //| Some cts -> match cts |> Array.filter (fun c -> c.ComponentID = cid) with
        //              | [||] -> None
        //              | l -> Some (l.[0])

    member me.TryGetComponentForEntities (cid:byte) (eids:uint32[]) = 
        me.GetEntitiesWithComponent cid
        |> Array.filter (fun e -> eids|>Array.contains e)
        |> Array.Parallel.map (fun e -> me.GetComponent cid e)


module History =

    let GetComponent (componentID:byte) (entities:Map<uint32,Component[]>) (entityID:uint32) =
        entities.Item(entityID)
        |> Array.find (fun x -> x.ComponentID = componentID)

    let GetEntitiesAtLocation (locationMap:Map<LocationDataInt,uint32[]>) (location:LocationDataInt) =
        match locationMap.ContainsKey location with
        | false -> [||]
        | true -> locationMap.Item location


