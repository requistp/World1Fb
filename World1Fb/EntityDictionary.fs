module EntityDictionary
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes


type ComponentDictionary() = 
    let mutable _compDict = Map.empty<ComponentTypes,uint32[]>
    member this.Components = _compDict
    member this.UpdateComponentDictionary (ents:Map<uint32,AbstractComponent[]>) =
        _compDict <- ents |> Map.fold (fun m k v -> v |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType k) m ) Map.empty<ComponentTypes,uint32[]>

        
type LocationDictionary() =
    let mutable _locDict = Map.empty<LocationDataInt,uint32[]>
    member this.Locations = _locDict
    member this.UpdateLocationDictionary (forms:FormComponent[]) =
        _locDict <- forms |> Array.fold (fun m f -> Map_AppendValueToArray m f.Location f.EntityID) Map.empty<LocationDataInt,uint32[]>


type DictionaryType = Current | Next

[<AbstractClass>]
type AbstractEntityDictionary(myType:DictionaryType) =
    let mutable _entities = Map.empty<uint32,AbstractComponent[]>

    member this.Entities = _entities
    member this.TryGet eid =
        _entities.ContainsKey(eid) |> TrueSomeFalseNone (_entities.Item(eid))
    member this.TryGetComponent (eid:uint32) (ct:ComponentTypes) = 
        let tryGetComponent (cts:AbstractComponent[]) = 
            match cts |> Array.filter (fun c -> c.ComponentType = ct) with
            | [||] -> None
            | l -> Some l.[0]
        eid |> this.TryGet |> Option.bind tryGetComponent

    member internal this.AddEntity (eid:uint32) (acs:AbstractComponent[]) = 
        match myType with
        | Current -> ()
        | Next -> _entities <- _entities.Add(eid,acs)
    member internal this.ReplaceComponent (eid:uint32) (acs:AbstractComponent[]) = 
        match myType with
        | Current -> ()
        | Next -> _entities <- Map_Replace _entities eid acs
    member internal this.SetEntities (e:Map<uint32,AbstractComponent[]>) = 
        match myType with
        | Next -> ()
        | Current -> _entities <- e

           
type NextEntityDictionary() =
    inherit AbstractEntityDictionary(Next)
    let mutable _maxEntityID = 0u
    member this.MaxEntityID = _maxEntityID
    member this.NewEntityID = _maxEntityID <- _maxEntityID + 1u; _maxEntityID
    member this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        let addEntities =
            scl.NewEntities 
            |> Array.filter (fun acs -> acs.Length > 0)
            |> Array.iter (fun acs -> this.AddEntity acs.[0].EntityID acs) // Can't Parallel            
        let componentChanges =
            let sumOfChangesDistinctByEntityAndComponent (map:Map<uint32*ComponentTypes,AbstractComponentChange>) (tup:uint32*ComponentTypes) (acc:AbstractComponentChange) =
                match map.ContainsKey(tup) with
                | false -> map.Add(tup,acc)
                | true -> let newItem = map.Item(tup).AddChange acc
                          map.Remove(tup).Add(tup,newItem)
            let changeComponent (accs:AbstractComponentChange[]) =
                let doTheChange (acc:AbstractComponentChange) = 
                    match acc.ComponentType |> this.TryGetComponent acc.EntityID with
                    | None -> ()
                    | Some (oc:AbstractComponent) -> 
                        this.Entities.Item(acc.EntityID) 
                        |> Array.filter (fun c -> c.ComponentType <> acc.ComponentType) 
                        |> Array.append [|acc.AddChange oc|]
                        |> this.ReplaceComponent acc.EntityID
                accs |> Array.iter (fun acc -> doTheChange acc) // Can't Parallel
            scl.ComponentChanges
            |> Array.fold (fun m acc -> sumOfChangesDistinctByEntityAndComponent m (acc.EntityID,acc.ComponentType) acc) Map.empty<uint32*ComponentTypes,AbstractComponentChange>
            |> Map.toArray
            |> Array.Parallel.map (fun tup -> snd tup)
            |> changeComponent
        addEntities
        componentChanges
        this.Entities


type CurrentEntityDictionary() =
    inherit AbstractEntityDictionary(Current)
    member this.SetEntities (e:Map<uint32,AbstractComponent[]>) = this.SetEntities e


type EntityDictionary() =
    let ents = new CurrentEntityDictionary()
    let compDict = new ComponentDictionary()
    let locDict = new LocationDictionary()
    let nextEnts = new NextEntityDictionary()

    member this.Components = compDict.Components
    member this.Entities = ents.Entities
    member this.Locations = locDict.Locations
    member this.MaxEntityID = nextEnts.MaxEntityID
    member this.NewEntityID = nextEnts.NewEntityID
    
    member this.GetComponent (ct:ComponentTypes) (eid:uint32) = 
        ents.Entities.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)

    member this.GetAllAndComponent (ct:ComponentTypes) =
        this.EntitiesWithComponent ct
        |> Array.Parallel.map (fun eid -> this.GetComponent ct eid)

    member this.EntitiesWithComponent (ct:ComponentTypes) =
        match compDict.Components.ContainsKey(ct) with
        | true -> compDict.Components.Item(ct)
        | false -> Array.empty
    
    member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        ents.SetEntities (nextEnts.ProcessSystemChangeLog scl)
        compDict.UpdateComponentDictionary ents.Entities
        locDict.UpdateLocationDictionary (this.GetAllAndComponent Form |> Array.Parallel.map (fun ac -> ac :?> FormComponent))
