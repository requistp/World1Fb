module EntityComponentManager
open AbstractComponent
open CommonGenericFunctions
open System

type EntityComponentData = {
    Entities : Map<uint32,AbstractComponent list>
    Components : Map<Byte,uint32 list>
    MaxEntityID : uint32
    } with 
    static member Empty = { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u}
    
module Entity =
    let private componentDictionary_AddComponent (cd:Map<Byte,uint32 list>) eid (ct:AbstractComponent) =
        match cd.ContainsKey(ct.ComponentID) with
        | false -> cd.Add(ct.ComponentID,[eid])
        | true -> let il = cd.Item(ct.ComponentID)
                  cd.Remove(ct.ComponentID).Add(ct.ComponentID,eid::il)
    let private componentDictionary_AddEntity (cd:Map<Byte,uint32 list>) eid (ctl:AbstractComponent list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.ContainsKey(ct.ComponentID) with
            | false -> newcd <- newcd.Add(ct.ComponentID,[eid])
            | true -> let il = newcd.Item(ct.ComponentID)
                      newcd <- newcd.Remove(ct.ComponentID).Add(ct.ComponentID,eid::il)
        newcd
    let private componentDictionary_RemoveComponent (cd:Map<Byte,uint32 list>) eid (ct:AbstractComponent) =
        match cd.Item(ct.ComponentID) |> List.exists (fun x -> x=eid) with
        | false -> cd
        | true -> let il = cd.Item(ct.ComponentID) |> List.filter (fun x -> x<>eid)
                  cd.Remove(ct.ComponentID).Add(ct.ComponentID,il)
    let private componentDictionary_RemoveEntity (cd:Map<Byte,uint32 list>) eid (ctl:AbstractComponent list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.Item(ct.ComponentID) |> List.exists (fun x -> x=eid) with
            | true -> let il = newcd.Item(ct.ComponentID) |> List.filter (fun x -> x<>eid)
                      newcd <- newcd.Remove(ct.ComponentID).Add(ct.ComponentID,il)
            | false -> ()
        newcd
    let private tryGetComponent cid (ctl:AbstractComponent list) = 
        match ctl |> List.filter (fun c -> c.ComponentID=cid) with
        | [] -> None
        | l -> Some l.Head

    let AllWithComponent ecd cid =
        match ecd.Components.ContainsKey(cid) with
        | true -> ecd.Components.Item(cid)
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

    let GetComponent ecd cid e = 
        ecd.Entities.Item(e) |> List.find (fun x -> x.ComponentID=cid)

    let Remove ecd e = 
        {
            Entities = ecd.Entities.Remove(e)
            Components = componentDictionary_RemoveEntity ecd.Components e (ecd.Entities.Item(e))
            MaxEntityID = ecd.MaxEntityID
        }

    let TryGet (entities:Map<uint32,AbstractComponent list>) eid =
        entities.ContainsKey(eid) |> TrueSomeFalseNone (entities.Item(eid))

    let ReplaceComponent (ecd:EntityComponentData) eid (newc:AbstractComponent) =
        match (TryGet ecd.Entities eid) with
        | None -> ecd
        | Some ctl -> let newEntities = 
                          ctl 
                          |> List.filter (fun ct -> ct.ComponentID<>newc.ComponentID) 
                          |> List.append [newc]
                          |> Map_Replace ecd.Entities eid  
                      { Entities = newEntities; Components = ecd.Components; MaxEntityID = ecd.MaxEntityID } //Replacing component won't change the componentDictionary

    let TryGetComponent (entities:Map<uint32,AbstractComponent list>) cid eid = 
        eid |> TryGet entities |> Option.bind (tryGetComponent cid)

