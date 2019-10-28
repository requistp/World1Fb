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

    member _.CreateEntity (cts:AbstractComponent[]) = 
        agentForEntities.CreateEntity cts
        agentForComponents.Add cts
        agentForLocations.Add cts
        Ok (Some "in async")

    member _.Exists eid = agentForEntities.Exists eid

    member _.GetComponents (eid:uint32) = agentForEntities.GetComponents eid

    member me.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T =
        (me.GetComponents eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T

    member _.GetEntitiesWithComponent ct = agentForComponents.Get ct

    member _.GetEntitiesAtLocation location = agentForLocations.Get location

    member _.GetMaxID = agentForID.GetMaxID

    member _.GetNewID = agentForID.GetNewID

    member _.HasAllComponents (cts:ComponentTypes[]) (eid:uint32) =
        let ects = agentForEntities.GetComponents eid |> Array.Parallel.map (fun ct -> ct.ComponentType)
        cts |> Array.forall (fun ct -> ects |> Array.contains ct)

    member _.PendingUpdates = 
        agentForEntities.PendingUpdates || agentForID.PendingUpdates || agentForComponents.PendingUpdates || agentForLocations.PendingUpdates

    member _.RemoveEntity (eid:uint32) =
        let cts = agentForEntities.GetComponents eid
        agentForComponents.Remove cts
        agentForLocations.Remove cts
        agentForEntities.RemoveEntity eid
        Ok (Some "in async")
        
    member me.ReplaceComponent (ac:AbstractComponent) (changes:string option) =
        if (ac.ComponentType = Component_Form) then 
            agentForLocations.Move (me.GetComponent<FormComponent> ac.EntityID) (ac :?> FormComponent)
        agentForEntities.ReplaceComponent ac
        Ok (Some "in async") //changes

    member _.TryGet (eid:uint32) =
        agentForEntities.Exists eid |> TrueSomeFalseNone (agentForEntities.GetComponents eid)

    member me.TryGetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : Option<'T> = 
        match me.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)

    member me.TryGetComponentForEntities<'T when 'T:>AbstractComponent> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> me.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)



