module EntityDictionary
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open LocationTypes


[<AbstractClass>]
type AbstractEntityDictionary() =
    let mutable _entities = Map.empty<uint32,AbstractComponent[]>
    let mutable _compDict = Map.empty<ComponentTypes,uint32[]>
    let mutable _locDict = Map.empty<LocationDataInt,uint32[]>

    member this.Components = _compDict
    member this.Entities = _entities
    member this.EntitiesAtLocation (l:LocationDataInt) = 
        match _locDict.ContainsKey(l) with
        | true -> _locDict.Item(l)
        | false -> Array.empty
    member this.EntitiesWithComponent (ct:ComponentTypes) =
        match _compDict.ContainsKey(ct) with
        | true -> _compDict.Item(ct)
        | false -> Array.empty
    member this.GetComponent<'T> (eid:uint32) : 'T =
        (_entities.Item(eid) |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T
    member this.GetComponent<'T> (eids:uint32[]) : 'T[] = 
        eids
        |> Array.map (fun e -> this.GetComponent<'T> e)
    member this.Locations = _locDict
    member this.TryGet eid =
        _entities.ContainsKey(eid) |> TrueSomeFalseNone (_entities.Item(eid))
    member this.TryGetComponent<'T> (eid:uint32) : Option<'T> = 
        match this.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)

    member internal this.CreateEntity (cts:AbstractComponent[]) : Result<string option,string> = 
        match _entities.ContainsKey(cts.[0].EntityID) with
        | true -> Error "CreateEntity: Entity already in dictionary"
        | false -> _entities <- _entities.Add(cts.[0].EntityID,cts)
                   this.UpdateComponentDictionary
                   this.UpdateLocationDictionary
                   Ok None
    member internal this.ReplaceComponent (ac:AbstractComponent) (changes:string option) : Result<string option,string> = 
        _entities <- _entities.Item(ac.EntityID)
                    |> Array.filter (fun c -> c.ComponentType <> ac.ComponentType) 
                    |> Array.append [|ac|]
                    |> Map_Replace _entities ac.EntityID
        //this.UpdateComponentDictionary Shouldn't have to update on a 1:1 component swap
        this.UpdateLocationDictionary
        Ok changes
    member internal this.Set (aed:AbstractEntityDictionary) : Result<string option,string> =
        _entities <- aed.Entities
        _compDict <- aed.Components
        _locDict <- aed.Locations
        Ok None

    member private this.UpdateComponentDictionary =
        _compDict <- _entities 
                     |> Map.fold (fun m k v -> v |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType k) m ) Map.empty<ComponentTypes,uint32[]>
    member private this.UpdateLocationDictionary =
        _locDict <- FormComponent.Type
                    |> this.EntitiesWithComponent 
                    |> Array.Parallel.map (fun eid -> this.GetComponent<FormComponent> eid)
                    |> Array.fold (fun m f -> Map_AppendValueToArray m f.Location f.EntityID) Map.empty<LocationDataInt,uint32[]>


type NextEntityDictionary() =
    inherit AbstractEntityDictionary()
    
    let mutable _maxEntityID = 0u

    member internal this.MaxEntityID = _maxEntityID
    member internal this.NewEntityID = _maxEntityID <- _maxEntityID + 1u; _maxEntityID

    member internal this.Set (aed:AbstractEntityDictionary) = Error "SetCurrentToNext: Called on Next Dictionary"


type EntityDictionary() =
    inherit AbstractEntityDictionary()

    let nextDict = new NextEntityDictionary()

    member this.NextEntityDictionary = nextDict

    member internal this.CreateEntity (cts:AbstractComponent[]) = Error "CreateEntity: Called against Current entity dictionary"
    member internal this.ReplaceComponent (ac:AbstractComponent) = Error "ReplaceComponent: Called against Current entity dictionary"
    member internal this.Set = base.Set nextDict



