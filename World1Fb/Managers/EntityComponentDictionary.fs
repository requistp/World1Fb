module EntityComponentDictionary
open AbstractComponent
open CommonGenericFunctions
open ComponentDictionary


type EntityComponentDictionary() = 
    let mutable _entDict = Map.empty<uint32,ComponentDictionary>

    member this.Copy (eid:uint32) (neweid:uint32) =
        this.List eid
        |> Array.Parallel.map (fun (ct:AbstractComponent) -> ct.Copy neweid)
    
    member this.CreateEntity (cts:AbstractComponent[]) =
        match _entDict.ContainsKey(cts.[0].EntityID) with
        | true -> ()
        | false -> 
            _entDict <- _entDict.Add(cts.[0].EntityID,ComponentDictionary(cts))

    member this.Exists (eid:uint32) = 
        _entDict.ContainsKey(eid)

    member this.GetComponent<'T when 'T:>AbstractComponent> (eid:uint32) : 'T =
        (this.List eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T

    member this.HasAllComponents (cts:'T[]) (eid:uint32) =
        cts 
        |> Array.forall (fun ct -> this.List eid |> Array.exists (fun ec -> ec.GetType() = ct))

    member this.List (eid:uint32) =
        _entDict.Item(eid).List

    member this.List () =
        _entDict |> Map.iter (fun k v -> printfn "%i | %i" k v.List.Length)

    member this.RemoveEntity (eid:uint32) =
        _entDict <- _entDict.Remove eid

    member this.ReplaceComponent (ct:AbstractComponent) =
        match _entDict.ContainsKey ct.EntityID with
        | false -> ()
        | true -> 
            _entDict.Item(ct.EntityID).Replace ct
        
    member this.TryGet eid =
        this.Exists eid |> TrueSomeFalseNone (this.List eid)

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



                           