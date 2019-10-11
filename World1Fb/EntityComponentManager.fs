module EntityComponentManager
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

    member private this.onEntityCreate (ge:AbstractGameEvent) =
        let ec = ge :?> Event_Entity_Create
        _maxEntityID <- _maxEntityID + 1u
        //eventQueueing _maxEntityID ctl
        _entitiesNext <- _entitiesNext.Add(_maxEntityID,ec.Components)
            
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

    member this.TryGetComponent ct eid = 
        eid |> this.TryGet |> Option.bind (tryGetComponent ct)
    
    member this.Initialize =
        evm.RegisterListener Entity_Create this.onEntityCreate 

