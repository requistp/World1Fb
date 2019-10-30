module EntityManager
open AbstractComponent
open CommonGenericFunctions
open ComponentEntityAgent
open EntityComponentAgent
open EntityIDAgent
open FormComponent
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

    member _.CreateEntity (e,cts:Component[]) = 
        agentForEntities.CreateEntity (e,cts)
        agentForComponents.Add cts
        agentForLocations.Add cts
        Ok (Some "in async")

    member _.Exists eid = agentForEntities.Exists eid

    member _.GetComponents (eid:uint32) = agentForEntities.GetComponents eid

    member me.GetComponent (cid:int) (eid:uint32) =
        me.GetComponents eid |> Array.find (fun x -> x.ComponentID = cid)

    member _.GetEntitiesWithComponent ct = agentForComponents.Get ct

    member _.GetEntitiesAtLocation location = agentForLocations.Get location

    member _.GetMaxID = agentForID.GetMaxID

    member _.GetNewID = agentForID.GetNewID

    member _.HasAllComponents (cts:int[]) (eid:uint32) =
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
            | Form (e,fd) ->
                let (Form (e,old)) = (me.GetComponent 1 e)
                agentForLocations.Move (e,old,fd) // (me.GetComponent 1 ac.EntityID) (ac :?> FormComponent)
            | _ -> ()
        handleComponentSpecificIssues

        let (Form (e,_)) = ac
        agentForEntities.ReplaceComponent (e,ac)
        Ok changes

    member _.TryGet (eid:uint32) =
        agentForEntities.Exists eid |> TrueSomeFalseNone (agentForEntities.GetComponents eid)

    member me.TryGetComponent (cid:int) (eid:uint32) : Option<Component> = 
        match me.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.ComponentID = cid) with
                      | [||] -> None
                      | l -> Some (l.[0])

    member me.TryGetComponentForEntities (cid:int) (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> me.TryGetComponent cid eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)



