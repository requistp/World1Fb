﻿module EntityManager
open agent_ComponentEntity
open agent_EntityComponent
open agent_EntityID
open agent_LocationEntity
open Component
open ComponentEnums
open CommonGenericFunctions


type EntityManager() =
    let agentForEntityID = new agent_EntityID()
    let agentForComponents = new agent_ComponentEntity()
    let agentForLocations = new agent_LocationEntity()
    let agentForEntities = new agent_EntityComponent() 

    member _.CopyEntity (oldeid:uint32) =
        let neweid = agentForEntityID.GetNewID
        oldeid
        |> agentForEntities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    member _.CreateEntity (cts:Component[]) = 
        Async.Parallel 
        (
            agentForEntities.CreateEntity (cts.[0].EntityID,cts)
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

    member _.GetEntitiesWithComponent c = agentForComponents.Get c

    member _.GetEntitiesAtLocation location = agentForLocations.Get location

    member _.GetMap = agentForEntities.Get

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
    
    member _.Init (maxEntityID:uint32) =
        agentForEntityID.Init maxEntityID

    member _.PendingUpdates = 
        agentForEntities.PendingUpdates || agentForEntityID.PendingUpdates || agentForComponents.PendingUpdates || agentForLocations.PendingUpdates

    member _.RemoveEntity (eid:uint32) =
        let cts = agentForEntities.GetComponents eid
        agentForComponents.Remove cts
        agentForLocations.Remove cts
        agentForEntities.RemoveEntity eid
        Ok None
        
    member me.ReplaceComponent (c:Component) =
        match c with
        | Form f -> f |> agentForLocations.Move (me.GetComponent FormComponentID c.EntityID).ToForm
        | _ -> ()
        agentForEntities.ReplaceComponent c

    member _.TryGet (eid:uint32) =
        agentForEntities.Exists eid |> TrueSomeFalseNone (agentForEntities.GetComponents eid)

    member me.TryGetComponent (cid:byte) (eid:uint32) : Option<Component> = 
        match me.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.ComponentID = cid) with
                      | [||] -> None
                      | l -> Some (l.[0])

    member me.TryGetComponentForEntities (cid:byte) (eids:uint32[]) = 
        me.GetEntitiesWithComponent cid
        |> Array.filter (fun e -> eids|>Array.contains e)
        |> Array.Parallel.map (fun e -> me.GetComponent cid e)

    member me.History_GetComponents (round:uint32) (eid:uint32) =
        //fix this
        me.GetComponents eid
