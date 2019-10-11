module EntityComponentManager
open AbstractComponent
open CommonGenericFunctions
open System


type EntityComponentData = 
    {
        Entities : Map<uint32,AbstractComponent[]>
        Components : Map<ComponentTypes,uint32[]>
        MaxEntityID : uint32
    } with 
    static member Empty = { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u}

    
module Entity =
    let private componentDictionary_AddComponent (cd:Map<ComponentTypes,uint32[]>) eid (ct:AbstractComponent) =
        match cd.ContainsKey(ct.ComponentType) with
        | false -> cd.Add(ct.ComponentType,[|eid|])
        | true -> let il = [|eid|] |> Array.append (cd.Item(ct.ComponentType)) 
                  cd.Remove(ct.ComponentType).Add(ct.ComponentType,il)
    let private componentDictionary_AddEntity (cd:Map<ComponentTypes,uint32[]>) eid (cts:AbstractComponent[]) =
        cts |> Array.fold (fun cd ct -> componentDictionary_AddComponent cd eid ct) cd
    let private componentDictionary_RemoveComponent (cd:Map<ComponentTypes,uint32[]>) eid (ct:AbstractComponent) =
        match cd.Item(ct.ComponentType) |> Array.exists (fun x -> x = eid) with
        | false -> cd
        | true -> let il = cd.Item(ct.ComponentType) |> Array.filter (fun x -> x <> eid)
                  cd.Remove(ct.ComponentType).Add(ct.ComponentType,il)
    let private componentDictionary_RemoveEntity (cd:Map<ComponentTypes,uint32[]>) eid (cts:AbstractComponent[]) =
        cts |> Array.fold (fun cd ct -> componentDictionary_RemoveComponent cd eid ct) cd
    let private tryGetComponent componentType (ctl:AbstractComponent[]) = 
        match ctl |> Array.filter (fun c -> c.ComponentType=componentType) with
        | [||] -> None
        | l -> Some l.[0]

    let AllWithComponent ecd componentType =
        match ecd.Components.ContainsKey(componentType) with
        | true -> ecd.Components.Item(componentType)
        | false -> Array.empty

    let Create ecd ctl =
        let eid = ecd.MaxEntityID + 1u
        {
            Entities = ecd.Entities.Add(eid,ctl)
            Components = componentDictionary_AddEntity ecd.Components eid ctl
            MaxEntityID = eid
        }

    let GetComponent ecd componentType eid = 
        ecd.Entities.Item(eid) |> Array.find (fun x -> x.ComponentType=componentType)

    let TryGet (entities:Map<uint32,AbstractComponent[]>) eid =
        entities.ContainsKey(eid) |> TrueSomeFalseNone (entities.Item(eid))

    let ReplaceComponent (ecd:EntityComponentData) eid (newc:AbstractComponent) =
        match (TryGet ecd.Entities eid) with
        | None -> ecd
        | Some ctl -> let newEntities = 
                          ctl 
                          |> Array.filter (fun ct -> ct.ComponentType<>newc.ComponentType) 
                          |> Array.append [|newc|]
                          |> Map_Replace ecd.Entities eid  
                      { Entities = newEntities; Components = ecd.Components; MaxEntityID = ecd.MaxEntityID } //Replacing component won't change the componentDictionary, so no work there

    let TryGetComponent (entities:Map<uint32,AbstractComponent[]>) componentType eid = 
        eid |> TryGet entities |> Option.bind (tryGetComponent componentType)

type EntityManager() =
    let mutable _componentDictionary = Map.empty<ComponentTypes,uint32[]>
    let mutable _entitiesCurrent = Map.empty<uint32,AbstractComponent[]>
    let mutable _entitiesNext = Map.empty<uint32,AbstractComponent[]>
    let mutable _maxEntityID = 0u

    let tryGetComponent ct (cts:AbstractComponent[]) = 
        match cts |> Array.filter (fun c -> c.ComponentType = ct) with
        | [||] -> None
        | l -> Some l.[0]

    //let private eventQueueing (eid:uint32) (ctl:AbstractComponent[]) =
    //    if ctl |> Array.exists (fun ct -> ct.ComponentType = Terrain)

    member this.GetAllWithComponent ct =
        match _componentDictionary.ContainsKey(ct) with
        | true -> _componentDictionary.Item(ct)
        | false -> Array.empty
        
    member this.GetComponent ct eid = 
        _entitiesCurrent.Item(eid) |> Array.find (fun x -> x.ComponentType = ct)
    
    member this.TryGet eid =
        _entitiesCurrent.ContainsKey(eid) |> TrueSomeFalseNone (_entitiesCurrent.Item(eid))

    member this.TryGetComponent ct eid = 
        eid |> this.TryGet |> Option.bind (tryGetComponent ct)
    
    // Next frame Entities -------------------------------------------------------------------------------------

    member this.CreateEntity cts =
        _maxEntityID <- _maxEntityID + 1u
        //eventQueueing _maxEntityID ctl
        _entitiesNext <- _entitiesNext.Add(_maxEntityID,cts)
        _entitiesNext

    member this.ReplaceComponent eid (newc:AbstractComponent) =
        match eid|>this.TryGet with
        | None -> _entitiesNext
        | Some cts -> cts 
                      |> Array.filter (fun ct -> ct.ComponentType <> newc.ComponentType) 
                      |> Array.append [|newc|]
                      |> Map_Replace _entitiesNext eid  

