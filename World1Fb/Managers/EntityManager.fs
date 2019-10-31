module EntityManager
open Component
open CommonGenericFunctions
open ComponentEntityAgent
open EntityComponentAgent
open EntityIDAgent
open LocationEntityAgent


type EntityManager() =
    let agentForID = new EntityIDAgent()
    let agentForComponents = new ComponentEntityAgent()
    let agentForLocations = new LocationEntityAgent()
    let agentForEntities = new EntityComponentAgent() 

    member _.CopyEntity (oldeid:uint32) (neweid:uint32) =
        oldeid
        |> agentForEntities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    member _.CreateEntity (cts:Component[]) = 
        agentForEntities.CreateEntity (cts.[0].EntityID,cts)
        agentForComponents.Add cts
        agentForLocations.Add cts
        Ok (Some "in async")

    member _.Exists eid = agentForEntities.Exists eid

    member _.GetComponents (eid:uint32) = agentForEntities.GetComponents eid

    member me.GetComponent (cid:byte) (eid:uint32) =
        me.GetComponents eid |> Array.find (fun x -> x.ComponentID = cid)

    member _.GetEntitiesWithComponent c = agentForComponents.Get c

    member _.GetEntitiesAtLocation location = agentForLocations.Get location

    member _.GetMaxID = agentForID.GetMaxID

    member _.GetNewID = agentForID.GetNewID

    member _.HasAllComponents (cts:byte[]) (eid:uint32) =
        let ects = agentForEntities.GetComponents eid |> Array.Parallel.map (fun ct -> ct.ComponentID)
        cts |> Array.forall (fun ct -> ects |> Array.contains ct)

    member _.PendingUpdates = 
        agentForEntities.PendingUpdates || agentForID.PendingUpdates || agentForComponents.PendingUpdates || agentForLocations.PendingUpdates

    member _.RemoveEntity (eid:uint32) =
        let cts = agentForEntities.GetComponents eid
        agentForComponents.Remove cts
        agentForLocations.Remove cts
        agentForEntities.RemoveEntity eid
        Ok (Some "in async")
        
    member me.ReplaceComponent (ac:Component) (changes:string option) =
        let handleComponentSpecificIssues =
            match ac with
            | Form fd ->
                let old = (me.GetComponent FormData.ID ac.EntityID).ToForm
                agentForLocations.Move (old,fd)
            | _ -> ()
        handleComponentSpecificIssues

        agentForEntities.ReplaceComponent ac
        Ok changes

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


