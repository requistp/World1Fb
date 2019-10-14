module EntityDictionary
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes


type DictionaryType = Current | Next

[<AbstractClass>]
type AbstractEntityDictionary(myType:DictionaryType) =
    let mutable _entities = Map.empty<uint32,AbstractComponent[]>
    let mutable _compDict = Map.empty<ComponentTypes,uint32[]>
    let mutable _locDict = Map.empty<LocationDataInt,uint32[]>

    member this.GetComponent (ct:ComponentTypes) (eid:uint32) = 
        _entities.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)    
    member this.Components = _compDict
    member this.Entities = _entities
    member this.Locations = _locDict
    member this.EntitiesAtLocation (l:LocationDataInt) = 
        match _locDict.ContainsKey(l) with
        | true -> _locDict.Item(l)
        | false -> Array.empty
    member this.EntitiesWithComponent (ct:ComponentTypes) =
        match _compDict.ContainsKey(ct) with
        | true -> _compDict.Item(ct)
        | false -> Array.empty
    member this.FormsAtLocation (l:LocationDataInt) =
        this.EntitiesAtLocation l
        |> Array.Parallel.map (fun eid -> (this.GetComponent Form eid) :?> FormComponent)
    member this.GetAllAndComponent (ct:ComponentTypes) =
        this.EntitiesWithComponent ct
        |> Array.Parallel.map (fun eid -> this.GetComponent ct eid)
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
    member internal this.SetEntities (aed:AbstractEntityDictionary) = 
        match myType with
        | Next -> ()
        | Current -> 
            _entities <- aed.Entities
            _compDict <- aed.Components
            _locDict <- aed.Locations
    member internal this.SetCompDict d =
        match myType with
        | Current -> ()
        | Next -> _compDict <- d
    member internal this.SetLocDict d =
        match myType with
        | Current -> ()
        | Next -> _locDict <- d


type NextEntityDictionary() =
    inherit AbstractEntityDictionary(Next)
    let mutable _maxEntityID = 0u
    member internal this.MaxEntityID = _maxEntityID
    member internal this.NewEntityID = _maxEntityID <- _maxEntityID + 1u; _maxEntityID

    member private this.SetAuxDictionaries =
        this.SetCompDict (this.Entities |> Map.fold (fun m k v -> v |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType k) m ) Map.empty<ComponentTypes,uint32[]>)

        this.SetLocDict (Form 
                          |> this.GetAllAndComponent 
                          |> Array.Parallel.map (fun ac -> ac :?> FormComponent)
                          |> Array.fold (fun m f -> Map_AppendValueToArray m f.Location f.EntityID) Map.empty<LocationDataInt,uint32[]>)
        

    member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        let addEntities =
            scl.NewEntities 
            |> Array.filter (fun acs -> acs.Length > 0)
            |> Array.iter (fun acs -> this.AddEntity acs.[0].EntityID acs) // Can't Parallel            
            this.SetAuxDictionaries
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
                this.SetAuxDictionaries
            scl.ComponentChanges
            |> Array.fold (fun m acc -> sumOfChangesDistinctByEntityAndComponent m (acc.EntityID,acc.ComponentType) acc) Map.empty<uint32*ComponentTypes,AbstractComponentChange>
            |> Map.toArray
            |> Array.Parallel.map (fun tup -> snd tup)
            |> changeComponent
        addEntities
        componentChanges
        this


type EntityDictionary() =
    inherit AbstractEntityDictionary(Current)

    let nextDict = new NextEntityDictionary()

    member this.Locations = this.Locations

    member this.MaxEntityID = nextDict.MaxEntityID
    member this.NewEntityID = nextDict.NewEntityID

    member this.ProcessSystemChangeLog (scl:SystemChangeLog) =
        this.SetEntities (nextDict.ProcessSystemChangeLog scl)

