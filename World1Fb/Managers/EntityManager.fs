module EntityManager
open AbstractComponent
open CommonGenericFunctions
open ComponentEntityDictionary
open FormComponent
open LocationEntityDictionary
open LocationTypes


[<AbstractClass>]
type AbstractEntityDictionary() =
    let mutable _compDict = new ComponentEntityDictionary()
    let mutable _entities = Map.empty<uint32,AbstractComponent[]>
    let mutable _locDict = new LocationEntityDictionary()

    member this.List() = _locDict.List()
    member this.Components = _compDict
    member this.Copy (eid:uint32) (neweid:uint32) =
        _entities.Item(eid) |> Array.Parallel.map (fun ct -> ct.Copy neweid)
    member this.Entities = _entities
    member this.EntitiesAtLocation (l:LocationDataInt) =
        _locDict.List l
    member this.EntityHasAllComponents (cts:'T[]) (eid:uint32) =
        cts |> Array.forall (fun ct -> _entities.Item eid |> Array.exists (fun ec -> ec.GetType() = ct))
    member this.EntitiesWithComponent (ct:ComponentTypes) = _compDict.List ct
    member this.Exists (eid:uint32) = _entities.ContainsKey eid
    member this.GetComponent<'T> (eid:uint32) : 'T =
        (_entities.Item(eid) |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T
    //member this.GetComponent<'T> (eids:uint32[]) : 'T[] = 
    //    eids
    //    |> Array.Parallel.map (fun e -> this.GetComponent<'T> e)
    member this.Locations = _locDict
    member this.TryGet eid =
        _entities.ContainsKey(eid) |> TrueSomeFalseNone (_entities.Item(eid))
    member this.TryGetComponent<'T> (eid:uint32) : Option<'T> = 
        match this.TryGet eid with
        | None -> None
        | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
                      | [||] -> None
                      | l -> Some (l.[0] :?> 'T)
    member this.TryGetComponentForEntities<'T> (eids:uint32[]) = 
        eids
        |> Array.Parallel.map (fun eid -> this.TryGetComponent<'T> eid)
        |> Array.filter (fun aco -> aco.IsSome)
        |> Array.Parallel.map (fun aco -> aco.Value)
        
    member internal this.CreateEntity (cts:AbstractComponent[]) : Result<string option,string> = 
        match _entities.ContainsKey(cts.[0].EntityID) with
        | true -> Error "CreateEntity: Entity already in dictionary"
        | false -> _entities <- _entities.Add(cts.[0].EntityID,cts)
                   _compDict.Add cts
                   _locDict.Add cts
                   Ok (Some "Created in next dictionary")
    member internal this.RemoveEntity (eid:uint32) : Result<string option,string> =
        _compDict.Remove (_entities.Item eid)
        _locDict.Remove (_entities.Item eid)
        _entities <- _entities.Remove eid
        Ok None
    member internal this.ReplaceComponent (ac:AbstractComponent) (changes:string option) : Result<string option,string> = 
        match ac.ComponentType = Component_Form with
        | false -> ()
        | true -> _locDict.Move (this.GetComponent<FormComponent> ac.EntityID) (ac :?> FormComponent)

        _entities <- 
            _entities.Item(ac.EntityID)
            |> Array.filter (fun c -> c.ComponentType <> ac.ComponentType) 
            |> Array.append [|ac|]
            |> Map_Replace _entities ac.EntityID
        Ok changes
    member internal this.SetToNext (next:AbstractEntityDictionary) : Result<string option,string> =
        _entities <- next.Entities
        _compDict <- next.Components
        _locDict <- next.Locations
        Ok None
    

type NextEntityDictionary() =
    inherit AbstractEntityDictionary()
    let mutable _maxEntityID = 0u

    member internal this.MaxEntityID = _maxEntityID
    member internal this.NewEntityID = _maxEntityID <- _maxEntityID + 1u; _maxEntityID
    member internal this.SetToNext (next:AbstractEntityDictionary) = Error "SetToNext: Called on Next Dictionary"


type EntityManager() =
    inherit AbstractEntityDictionary()
    let mutable _maxEntityID = 0u
    let next = new NextEntityDictionary()

    member internal this.MaxEntityID = next.MaxEntityID
    member internal this.NewEntityID = next.NewEntityID
    member internal this.Next = next
    
    member internal this.CreateEntity (cts:AbstractComponent[]) = Error "CreateEntity: Called on Current Dictionary"
    member internal this.RemoveEntity (eid:uint32) = Error "RemoveEntity: Called on Current Dictionary"
    member internal this.ReplaceComponent (ac:AbstractComponent) = Error "ReplaceComponent: Called on Current Dictionary"
    member internal this.SetToNext = base.SetToNext next

