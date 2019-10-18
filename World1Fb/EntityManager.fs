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
    member _.GetComponent<'T> (eid:uint32) : 'T = entDict.GetComponent<'T> eid
    member _.GetComponent<'T> (eids:uint32[]) : 'T[] = entDict.GetComponent<'T> eids
    member _.Locations = entDict.Locations
    member _.MaxEntityID = entDict.NextEntityDictionary.MaxEntityID
    member _.NewEntityID = entDict.NextEntityDictionary.NewEntityID
    member _.NextEntityDictionary = entDict.NextEntityDictionary
    member _.SetToNext = entDict.Set
    member _.TryGet eid = entDict.TryGet eid
    member _.TryGetComponent<'T> eid = entDict.TryGetComponent<'T> eid

    member this.EntityHasAllComponents (cts:'T[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> entDict.Entities.Item eid |> Array.exists (fun ec -> ec.GetType() = ct))

    member this.Initialize =
        ()

