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

    let tryGetComponent ct (cts:AbstractComponent[]) = 
        match cts |> Array.filter (fun c -> c.ComponentType = ct) with
        | [||] -> None
        | l -> Some l.[0]

    //let eventQueueing (eid:uint32) (ctl:AbstractComponent[]) =
    //    if ctl |> Array.exists (fun ct -> ct.ComponentType = Terrain)

    member private this.entityCreate (acs:AbstractComponent[][]) =
        let addNextEntity cs =
            _maxEntityID <- _maxEntityID + 1u
            _entitiesNext <- _entitiesNext.Add(_maxEntityID,cs)
        acs |> Array.iter (fun cs -> addNextEntity cs)
        //eventQueueing _maxEntityID ctl

    member private this.componentChange (accs:AbstractComponentChange[]) =
        let doTheChange (acc:AbstractComponentChange) = 
            match acc.ComponentType |> this.TryGetComponent acc.EntityID with
            | None -> ()
            | Some (oc:AbstractComponent) -> 
                _entitiesNext <- _entitiesNext.Item(acc.EntityID) 
                                 |> Array.filter (fun c -> c.ComponentType <> acc.ComponentType) 
                                 |> Array.append [|acc.AddChange oc|]
                                 |> Map_Replace _entitiesNext acc.EntityID
        accs |> Array.iter (fun acc -> doTheChange acc)

    member private this.onSystemChangeLog (age:AbstractGameEvent) =
        let e = (age :?> Event_SystemChangeLog).SCL

        let mapadd (map:Map<uint32*ComponentTypes,AbstractComponentChange>) (tup:uint32*ComponentTypes) (acc:AbstractComponentChange) =
            match map.ContainsKey(tup) with
            | false -> map.Add(tup,acc)
            | true -> let newItem = map.Item(tup).AddChange acc
                      map.Remove(tup).Add(tup,newItem)
                
        e.ComponentChanges
        |> Array.fold (fun map acc -> mapadd map (acc.EntityID,acc.ComponentType) acc) Map.empty
        |> Map.toArray
        |> Array.map (fun tup -> snd tup)
        |> this.componentChange

        this.entityCreate e.NewEntities

    member private this.updatedComponentDictionary = 
        let addComponents (m:Map<ComponentTypes,uint32[]>) (cs:AbstractComponent[]) (eid:uint32) =
            cs |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType eid) m

        _entitiesCurrent |> Map.fold (fun m key value -> addComponents m value key) Map.empty<ComponentTypes,uint32[]>
        
    member this.Entities = _entitiesCurrent

    member this.GetAllWithComponent ct =
        match _componentDictionary.ContainsKey(ct) with
        | true -> _componentDictionary.Item(ct)
        | false -> Array.empty
        
    member this.GetComponent ct eid = 
        _entitiesCurrent.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)
    
    member this.SetCurrentToNext =
        _entitiesCurrent <- _entitiesNext
        _componentDictionary <- this.updatedComponentDictionary
        
    member this.TryGet eid =
        _entitiesCurrent.ContainsKey(eid) |> TrueSomeFalseNone (_entitiesCurrent.Item(eid))

    member this.TryGetComponent (eid:uint32) (ct:ComponentTypes) = 
        eid |> this.TryGet |> Option.bind (tryGetComponent ct)
    
    member this.Initialize =
        evm.RegisterListener GameEvents.SystemChangeLog this.onSystemChangeLog 

