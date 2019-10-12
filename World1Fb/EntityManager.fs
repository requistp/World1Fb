module EntityManager
open AbstractComponent
open CommonGenericFunctions
open EventManager
open GameEvents
open LocationTypes
open TerrainComponent

type EntityManager(evm:EventManager) =
    let mutable _componentDictionary = Map.empty<ComponentTypes,uint32[]>
    let mutable _entitiesCurrent = Map.empty<uint32,AbstractComponent[]>
    let mutable _entitiesNext = Map.empty<uint32,AbstractComponent[]>
    let mutable _locationDictionary = Map.empty<LocationDataInt,uint32[]>
    let mutable _maxEntityID = 0u

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
                let setEntityID (eid:uint32) =
                    cs |> Array.map (fun (c:AbstractComponent) -> if c.EntityID<>eid then c else c.NewWithEID eid)
                _maxEntityID <- _maxEntityID + 1u
                _entitiesNext <- _entitiesNext.Add(_maxEntityID,setEntityID _maxEntityID)
            scl.NewEntities 
            |> Array.iter (fun cs -> addNextEntity cs)
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
            let updatedComponentDictionary = 
                let addComponents (m:Map<ComponentTypes,uint32[]>) (eid:uint32) (cs:AbstractComponent[]) =
                    cs |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType eid) m
                _entitiesNext |> Map.fold (fun m key value -> addComponents m key value) Map.empty<ComponentTypes,uint32[]>
            let updatedLocationDictionary = 
                //let addLocations (m:Map<LocationDataInt,uint32[]>) (eid:uint32) (cs:AbstractComponent[]) =
                //    for 
                //    |> Array.filter (fun ct -> ct.ComponentType )
                // Map_AppendValueToArray
                Map.empty<LocationDataInt,uint32[]>

//member private this.onTerrainCreated (age:AbstractGameEvent) =
//    let e = (age :?> Event_TerrainCreated).Terrain
//    _terrainMap <- match _terrainMap.ContainsKey(e.Location) with
//                   | false -> _terrainMap.Add(e.Location,e.EntityID)
//                   | true -> _terrainMap.Remove(e.Location).Add(e.Location,e.EntityID)


            _entitiesCurrent <- _entitiesNext
            _componentDictionary <- updatedComponentDictionary
            _locationDictionary <- updatedLocationDictionary
        let handleNewEntityEvents =
            let handleEvent (ct:AbstractComponent) =
                match ct.ComponentType with
                | Terrain -> () //evm.QueueEvent(Event_TerrainCreated(ct :?> TerrainComponent))
                | _ -> ()
            scl.NewEntities
            |> Array.iter (fun cts -> cts |> Array.iter (fun ct -> handleEvent ct))

        addEntities
        componentChanges
        updateCurrentToNext
        handleNewEntityEvents
        (_entitiesCurrent,_maxEntityID)

