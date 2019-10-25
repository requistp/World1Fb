module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EntityDictionary
open FormComponent
open LocationTypes
open TerrainComponent


type EntityManager() =
    let entDict = new EntityDictionary()

    member _.Components = entDict.Components
    member _.Copy eid neweid = entDict.Copy eid neweid
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
    member _.TryGetComponentForEntities<'T> (eids:uint32[]) = entDict.TryGetComponentForEntities<'T> eids

    member this.EntityHasAllComponents (cts:'T[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> entDict.Entities.Item eid |> Array.exists (fun ec -> ec.GetType() = ct))
    member this.Exists (eid:uint32) = entDict.Entities.ContainsKey eid

    member this.ToDisplayString =
        let mutable s = ""
        for y in [0..MapHeight-1] do
            for x in [0..MapWidth-1] do
                let fs = 
                    entDict.EntitiesAtLocation { X = x; Y = y; Z = 0 } 
                    |> entDict.TryGetComponentForEntities<FormComponent> 
                let f = fs.[fs.Length-1]
                s <- s + f.Symbol.ToString()
            s <- s + "\n"
        s



        (*
        not sure this works...
        member this.HasComponent<'T> (eid:uint32) : bool =
            match this.TryGetComponent<'T> eid with
            | None -> false
            | Some _ -> true
            *)