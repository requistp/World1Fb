module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EntityComponentAgent
open LocationTypes

type EntityManager() =
    let entities = new EntityComponentAgent() 

    member _.EntityID_Max = entities.GetMaxID
    member _.EntityID_New = entities.GetNewID
    member _.Exists (eid:uint32) = entities.Exists eid
    member _.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T = entities.GetComponent<'T> eid
    member _.GetEntitiesWithComponent ct = entities.GetWithComponent ct
    member _.GetEntitiesAtLocation (location:LocationDataInt) = entities.GetLocation location
    member _.PendingUpdates = entities.PendingUpdates

    member this.CopyEntity (oldeid:uint32) (neweid:uint32) =
        oldeid
        |> entities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    member _.CreateEntity (cts:AbstractComponent[]) = 
        entities.CreateEntity cts
    
    member _.GetComponents (eid:uint32) = 
        entities.GetComponents eid

    member this.HasAllComponents (cts:ComponentTypes[]) (eid:uint32) =
        let ects = entities.GetComponents eid |> Array.Parallel.map (fun ct -> ct.ComponentType)
        cts |> Array.forall (fun ct -> ects |> Array.contains ct)
        
    member _.RemoveEntity (eid:uint32) =
        entities.RemoveEntity eid

    member _.ReplaceComponent (ac:AbstractComponent) (changes:string option) =
        entities.ReplaceComponent ac changes
        
    member this.TryGet (eid:uint32) =
        entities.Exists eid |> TrueSomeFalseNone (entities.GetComponents eid)

    member this.TryGetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : Option<'T> = 
        match this.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)

    member this.TryGetComponentForEntities<'T when 'T:>AbstractComponent> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> this.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)



