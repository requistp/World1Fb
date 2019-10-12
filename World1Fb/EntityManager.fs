module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EventManager
open GameEvents
open System

type EntityManager(evm:EventManager) =
    let mutable _componentDictionary = Map.empty<ComponentTypes,uint32[]>
    let mutable _entitiesCurrent = Map.empty<uint32,AbstractComponent[]>
    let mutable _entitiesNext = Map.empty<uint32,AbstractComponent[]>
    let mutable _maxEntityID = 0u

    //let eventQueueing (eid:uint32) (ctl:AbstractComponent[]) =
    //    if ctl |> Array.exists (fun ct -> ct.ComponentType = Terrain)

    member this.GetAllWithComponent ct =
        match _componentDictionary.ContainsKey(ct) with
        | true -> _componentDictionary.Item(ct)
        | false -> Array.empty
        
    member this.GetComponent ct eid = 
        _entitiesCurrent.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)
    
    member this.TryGet eid =
        _entitiesCurrent.ContainsKey(eid) |> TrueSomeFalseNone (_entitiesCurrent.Item(eid))

    member this.TryGetComponent eid ct = 
        let tryGetComponent (cts:AbstractComponent[]) = 
            match cts |> Array.filter (fun c -> c.ComponentType = ct) with
            | [||] -> None
            | l -> Some l.[0]
        eid |> this.TryGet |> Option.bind tryGetComponent
    
    member this.Initialize =
        ()
    
    member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        let addEntities =
            let addNextEntity cs =
                _maxEntityID <- _maxEntityID + 1u
                _entitiesNext <- _entitiesNext.Add(_maxEntityID,cs)
            scl.NewEntities |> Array.iter (fun cs -> addNextEntity cs)
        let componentChanges =
            let updateEntityComponentChangeMap (map:Map<uint32*ComponentTypes,AbstractComponentChange>) (tup:uint32*ComponentTypes) (acc:AbstractComponentChange) =
                match map.ContainsKey(tup) with
                | false -> map.Add(tup,acc)
                | true -> let newItem = map.Item(tup).AddChange acc
                          map.Remove(tup).Add(tup,newItem)
            let changeComponent (accs:AbstractComponentChange[]) =
                let doTheChange (acc:AbstractComponentChange) = 
                    match acc.ComponentType |> this.TryGetComponent acc.EntityID with
                    | None -> ()
                    | Some (oc:AbstractComponent) -> 
                        _entitiesNext <- _entitiesNext.Item(acc.EntityID) 
                                         |> Array.filter (fun c -> c.ComponentType <> acc.ComponentType) 
                                         |> Array.append [|acc.AddChange oc|]
                                         |> Map_Replace _entitiesNext acc.EntityID
                accs |> Array.iter (fun acc -> doTheChange acc)
            scl.ComponentChanges
            |> Array.fold (fun map acc -> updateEntityComponentChangeMap map (acc.EntityID,acc.ComponentType) acc) Map.empty
            |> Map.toArray
            |> Array.map (fun tup -> snd tup)
            |> changeComponent
        let updateCurrentToNext = 
            let addComponents (m:Map<ComponentTypes,uint32[]>) (eid:uint32) (cs:AbstractComponent[]) =
                cs |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType eid) m
            _entitiesCurrent <- _entitiesNext
            _componentDictionary <- _entitiesCurrent |> Map.fold (fun m key value -> addComponents m key value) Map.empty<ComponentTypes,uint32[]>

        addEntities
        componentChanges
        updateCurrentToNext
        (_entitiesCurrent,_maxEntityID)

