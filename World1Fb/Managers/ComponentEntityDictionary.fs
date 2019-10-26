module ComponentEntityDictionary
open AbstractComponent
open EntityDictionary


type ComponentEntityDictionary() = 
    let _compDict =
        ComponentTypes.AsArray
        |> Array.fold (fun (m:Map<ComponentTypes,EntityDictionary>) ct -> m.Add(ct,new EntityDictionary())) Map.empty

    member this.Add (ct:AbstractComponent) =
        (_compDict.Item ct.ComponentType).Add ct.EntityID

    member this.Add (cts:AbstractComponent[]) =
        cts |> Array.Parallel.iter (fun ct -> (_compDict.Item ct.ComponentType).Add ct.EntityID)

    member this.Remove (ct:AbstractComponent) =
        (_compDict.Item ct.ComponentType).Remove ct.EntityID

    member this.Remove (cts:AbstractComponent[]) =
        cts |> Array.Parallel.iter (fun ct -> (_compDict.Item ct.ComponentType).Remove ct.EntityID)
        
    member this.List (componentType:ComponentTypes) =
        _compDict.Item(componentType).List

    member this.List () =
        _compDict |> Map.iter (fun k v -> printfn "%s | %i" (k.ToString()) v.List.Length)
        
