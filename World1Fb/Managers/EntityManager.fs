module EntityManager
open AbstractComponent
open CommonGenericFunctions
open ComponentEntityDictionary
open EntityComponentDictionary
open FormComponent
open LocationEntityDictionary
open LocationTypes


[<AbstractClass>]
type AbstractEntityDictionary() =
    let mutable _compDict = new ComponentEntityDictionary()
    let mutable _entities = new EntityComponentDictionary() 
    let mutable _locDict = new LocationEntityDictionary()

    member this.Components = _compDict
    member this.Entities = _entities
    member this.Locations = _locDict
    
         
    member internal this.CreateEntity (cts:AbstractComponent[]) = 
        _entities.CreateEntity cts
        _compDict.Add cts
        _locDict.Add cts
        Ok None
    member internal this.RemoveEntity (eid:uint32) =
        _compDict.Remove (_entities.List eid)
        _locDict.Remove (_entities.List eid)
        _entities.RemoveEntity eid
        Ok None
    member internal this.ReplaceComponent (ac:AbstractComponent) (changes:string option) =
        match ac.ComponentType = Component_Form with
        | false -> ()
        | true -> _locDict.Move (_entities.GetComponent<FormComponent> ac.EntityID) (ac :?> FormComponent)
        _entities.ReplaceComponent ac
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
    let next = new NextEntityDictionary()

    member internal this.MaxEntityID = next.MaxEntityID
    member internal this.NewEntityID = next.NewEntityID
    member internal this.Next = next
    
    member internal this.CreateEntity (cts:AbstractComponent[]) = Error "CreateEntity: Called on Current Dictionary"
    member internal this.RemoveEntity (eid:uint32) = Error "RemoveEntity: Called on Current Dictionary"
    member internal this.ReplaceComponent (ac:AbstractComponent) = Error "ReplaceComponent: Called on Current Dictionary"
    member internal this.SetToNext = base.SetToNext next



//member this.EntityHasAllComponents (cts:'T[]) (eid:uint32) =
//    cts 
//    |> Array.forall (fun ct -> _entities.List eid |> Array.exists (fun ec -> ec.GetType() = ct))
//member this.TryGetComponentForEntities<'T> (eids:uint32[]) = 
//    eids
//    |> Array.Parallel.map (fun eid -> _entities.TryGetComponent<'T> eid)
//    |> Array.filter (fun aco -> aco.IsSome)
//    |> Array.Parallel.map (fun aco -> aco.Value)
//member this.TryGetComponent<'T> (eid:uint32) : Option<'T> = 
//    match _entities.TryGet eid with
//    | None -> None
//    | Some cts -> match cts |> Array.filter (fun c -> c.GetType() = typeof<'T>) with
//                  | [||] -> None
//                  | l -> Some (l.[0] :?> 'T)
//member this.TryGet eid =
//    _entities.Exists eid |> TrueSomeFalseNone (_entities.List eid)
//member this.GetComponent<'T> (eids:uint32[]) : 'T[] = 
//    eids
//    |> Array.Parallel.map (fun e -> this.GetComponent<'T> e)
    
//member this.GetComponent<'T> (eid:uint32) : 'T =
//    (_entities.List eid |> Array.find (fun x -> x.GetType() = typeof<'T>)) :?> 'T
//member this.EntitiesAtLocation (l:LocationDataInt) = _locDict.List l
//member this.Copy (eid:uint32) (neweid:uint32) =
//    _entities.List eid
//    |> Array.Parallel.map (fun ct -> ct.Copy neweid)    
//member this.EntitiesWithComponent (ct:ComponentTypes) = _compDict.EntitiesWithComponent ct
    