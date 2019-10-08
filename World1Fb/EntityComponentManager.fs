module EntityComponentManager
open AbstractComponent
open CommonGenericFunctions
open System

type EntityComponentData = {
    Entities : Map<uint32,AbstractComponent list>
    Components : Map<ComponentTypes,uint32 list>
    MaxEntityID : uint32
    } with 
    static member Empty = { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u}
    
module Entity =
    let private componentDictionary_AddComponent (cd:Map<ComponentTypes,uint32 list>) eid (ct:AbstractComponent) =
        match cd.ContainsKey(ct.ComponentType) with
        | false -> cd.Add(ct.ComponentType,[eid])
        | true -> let il = cd.Item(ct.ComponentType)
                  cd.Remove(ct.ComponentType).Add(ct.ComponentType,eid::il)
    let private componentDictionary_AddEntity (cd:Map<ComponentTypes,uint32 list>) eid (ctl:AbstractComponent list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.ContainsKey(ct.ComponentType) with
            | false -> newcd <- newcd.Add(ct.ComponentType,[eid])
            | true -> let il = newcd.Item(ct.ComponentType)
                      newcd <- newcd.Remove(ct.ComponentType).Add(ct.ComponentType,eid::il)
        newcd
    let private componentDictionary_RemoveComponent (cd:Map<ComponentTypes,uint32 list>) eid (ct:AbstractComponent) =
        match cd.Item(ct.ComponentType) |> List.exists (fun x -> x=eid) with
        | false -> cd
        | true -> let il = cd.Item(ct.ComponentType) |> List.filter (fun x -> x<>eid)
                  cd.Remove(ct.ComponentType).Add(ct.ComponentType,il)
    let private componentDictionary_RemoveEntity (cd:Map<ComponentTypes,uint32 list>) eid (ctl:AbstractComponent list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.Item(ct.ComponentType) |> List.exists (fun x -> x=eid) with
            | true -> let il = newcd.Item(ct.ComponentType) |> List.filter (fun x -> x<>eid)
                      newcd <- newcd.Remove(ct.ComponentType).Add(ct.ComponentType,il)
            | false -> ()
        newcd
    let private tryGetComponent componentType (ctl:AbstractComponent list) = 
        match ctl |> List.filter (fun c -> c.ComponentType=componentType) with
        | [] -> None
        | l -> Some l.Head

    let AllWithComponent ecd componentType =
        match ecd.Components.ContainsKey(componentType) with
        | true -> ecd.Components.Item(componentType)
        | false -> []

    let Create ecd ctl =
        let i = ecd.MaxEntityID + 1u
        {
            Entities = ecd.Entities.Add(i,ctl)
            Components = componentDictionary_AddEntity ecd.Components i ctl
            MaxEntityID = i
        }

    let EmptyECD = 
        {
            Entities = Map.empty
            Components = Map.empty
            MaxEntityID = 0u 
        }

    let GetComponent ecd componentType eid = 
        ecd.Entities.Item(eid) |> List.find (fun x -> x.ComponentType=componentType)

    let Remove ecd eid = 
        {
            Entities = ecd.Entities.Remove(eid)
            Components = componentDictionary_RemoveEntity ecd.Components eid (ecd.Entities.Item(eid))
            MaxEntityID = ecd.MaxEntityID
        }

    let TryGet (entities:Map<uint32,AbstractComponent list>) eid =
        entities.ContainsKey(eid) |> TrueSomeFalseNone (entities.Item(eid))

    let ReplaceComponent (ecd:EntityComponentData) eid (newc:AbstractComponent) =
        match (TryGet ecd.Entities eid) with
        | None -> ecd
        | Some ctl -> let newEntities = 
                          ctl 
                          |> List.filter (fun ct -> ct.ComponentType<>newc.ComponentType) 
                          |> List.append [newc]
                          |> Map_Replace ecd.Entities eid  
                      { Entities = newEntities; Components = ecd.Components; MaxEntityID = ecd.MaxEntityID } //Replacing component won't change the componentDictionary, so no work there

    let TryGetComponent (entities:Map<uint32,AbstractComponent list>) componentType eid = 
        eid |> TryGet entities |> Option.bind (tryGetComponent componentType)

