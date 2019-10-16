﻿module EntityDictionary
open AbstractComponent
open CommonGenericFunctions
open FormComponent
open EventManager
open LocationTypes


type DictionaryType = Current | Next

[<AbstractClass>]
type AbstractEntityDictionary(myType:DictionaryType) =
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
    member this.GetComponent (ct:ComponentTypes) (eid:uint32) = 
        _entities.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)    
    member this.Locations = _locDict
    member this.TryGet eid =
        _entities.ContainsKey(eid) |> TrueSomeFalseNone (_entities.Item(eid))
    member this.TryGetComponent (ct:ComponentTypes) (eid:uint32) = 
        let tryGetComponent (cts:AbstractComponent[]) = 
            match cts |> Array.filter (fun c -> c.ComponentType = ct) with
            | [||] -> None
            | l -> Some l.[0]
        eid |> this.TryGet |> Option.bind tryGetComponent

    member internal this.CreateEntity (cts:AbstractComponent[]) : Result<string option,string> = 
        match myType with
        | Current -> Error "CreateEntity: Called against Current entity dictionary"
        | Next -> match _entities.ContainsKey(cts.[0].EntityID) with
                  | true -> Error "CreateEntity: Entity already in dictionary"
                  | false -> _entities <- _entities.Add(cts.[0].EntityID,cts)
                             this.UpdateComponentDictionary
                             this.UpdateLocationDictionary
                             Ok None
    member internal this.ReplaceComponent (eid:uint32) (ac:AbstractComponent) = 
        match myType with
        | Current -> ()
        | Next -> _entities <- _entities.Item(eid)
                               |> Array.filter (fun c -> c.ComponentType <> ac.ComponentType) 
                               |> Array.append [|ac|]
                               |> Map_Replace _entities eid
                  this.UpdateComponentDictionary
                  this.UpdateLocationDictionary
    member internal this.SetCurrentToNext (aed:AbstractEntityDictionary) : Result<string option,string> =
        match myType with
        | Next -> Error "SetCurrentToNext: Called on Next Dictionary"
        | Current -> _entities <- aed.Entities
                     _compDict <- aed.Components
                     _locDict <- aed.Locations
                     Ok None

    member private this.UpdateComponentDictionary =
        _compDict <- _entities 
                     |> Map.fold (fun m k v -> v |> Array.fold (fun m c -> Map_AppendValueToArray m c.ComponentType k) m ) Map.empty<ComponentTypes,uint32[]>
    member private this.UpdateLocationDictionary =
        _locDict <- Form
                    |> this.EntitiesWithComponent 
                    |> Array.Parallel.map (fun eid -> (this.GetComponent Form eid) :?> FormComponent)
                    |> Array.fold (fun m f -> Map_AppendValueToArray m f.Location f.EntityID) Map.empty<LocationDataInt,uint32[]>


type NextEntityDictionary() =
    inherit AbstractEntityDictionary(Next)

    let mutable _maxEntityID = 0u

    member internal this.MaxEntityID = _maxEntityID
    member internal this.NewEntityID = _maxEntityID <- _maxEntityID + 1u; _maxEntityID

    //member internal this.CreateEntity (cts:AbstractComponent[]) : Result<string option,string> = 
      //  this.CreateEntity cts
    //member internal this.CreateEntity (cts:AbstractComponent[]) = this.CreateEntity cts
    //member internal this.ProcessSystemChangeLog (scl:SystemChangeLog) =
    //    let addEntities =
    //        scl.NewEntities 
    //        |> Array.filter (fun acs -> acs.Length > 0)
    //        |> Array.iter (fun acs -> this.AddEntity acs.[0].EntityID acs) // Can't Parallel      
    //    let componentChanges =
    //        let applyChange (acc:AbstractComponentChange) = 
    //            match this.TryGetComponent acc.ComponentType acc.EntityID with
    //            | None -> acc.Invalidate "Cannot locate EnitityID"
    //            | Some (ac:AbstractComponent) -> 
    //                let changedc = acc.AddChange ac
    //                match this.ValidateComponentChange changedc with
    //                | Some s -> acc.Invalidate s
    //                | None -> this.ReplaceComponent acc.EntityID changedc
    //                          acc
    //        let results = 
    //            scl.ComponentChanges
    //            |> Array.sortBy (fun c -> c.EntityID)
    //            |> Array.map (fun acc -> applyChange acc) // Can't Parallel
    //        { scl with ChangeResults = results }
    //    addEntities
    //    componentChanges

    //member private this.ValidateComponentChange (newc:AbstractComponent) =
    //    let testForImpassableFormAtLocation z =
    //        let formImpassableAtLocation (l:LocationDataInt) =
    //            l
    //            |> this.EntitiesAtLocation
    //            |> Array.Parallel.map (fun eid -> (this.GetComponent Form eid) :?> FormComponent)
    //            |> Array.exists (fun f -> not f.IsPassable)
    //        match newc.ComponentType=Form && formImpassableAtLocation (newc:?>FormComponent).Location with
    //        | true -> Some "Object at location"
    //        | false -> None

    //    None
    //    |> OptionBindNone testForImpassableFormAtLocation
        //|> OptionBindNone testForImpassableFormAtLocation
        //|> OptionBindNone testForImpassableFormAtLocation
        //|> OptionBindNone testForImpassableFormAtLocation
        //|> OptionBindNone testForImpassableFormAtLocation
        // Additional test here


type EntityDictionary() =
    inherit AbstractEntityDictionary(Current)

    let nextDict = new NextEntityDictionary()

    member this.CreateEntity cts = nextDict.CreateEntity cts
    member this.MaxEntityID = nextDict.MaxEntityID
    member this.NewEntityID = nextDict.NewEntityID
    member this.SetCurrentToNext = base.SetCurrentToNext nextDict

    //member this.ProcessSystemChangeLog (scl:SystemChangeLog) =
    //    let finalSCL = nextDict.ProcessSystemChangeLog scl
    //    this.SetEntities nextDict
    //    finalSCL

