module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EntityDictionary
open FormComponent
open LocationTypes

type EntityManager() =
    let entDict = new EntityDictionary()

    member _.Components = entDict.Components
    member _.Entities = entDict.Entities
    member _.EntitiesAtLocation l = entDict.EntitiesAtLocation l
    member _.EntitiesWithComponent ct = entDict.EntitiesWithComponent ct
    member _.GetComponent<'T> (eid:uint32) : 'T = entDict.GetComponent eid
    member _.Locations = entDict.Locations
    member _.MaxEntityID = entDict.NextEntityDictionary.MaxEntityID
    member _.NewEntityID = entDict.NextEntityDictionary.NewEntityID
    member _.NextEntityDictionary = entDict.NextEntityDictionary
    member _.SetToNext = entDict.Set
    member _.TryGet eid = entDict.TryGet eid
    member _.TryGetComponent<'T> eid = entDict.TryGetComponent<'T> eid

    member this.EntityHasAllComponents (cts:ComponentTypes[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> entDict.Entities.Item eid |> Array.exists (fun ec -> ec.ComponentType = ct))
    member this.TryGetComponentForEntities<'T> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> entDict.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)

    member this.Initialize =
        ()

