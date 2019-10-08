module EntityComponentManager
open Components
open System
open CommonGenericFunctions

type EntityComponentData = {
    Entities : Map<uint32,ComponentType list>
    Components : Map<Byte,uint32 list>
    MaxEntityID : uint32
    } with 
    static member Empty = { Entities=Map.empty; Components=Map.empty; MaxEntityID=0u}
    
module Entity =
    let private componentDictionary_AddComponent (cd:Map<Byte,uint32 list>) i (ct:ComponentType) =
        match cd.ContainsKey(ct.ComponentID) with
        | false -> cd.Add(ct.ComponentID,[i])
        | true -> let il = cd.Item(ct.ComponentID)
                  cd.Remove(ct.ComponentID).Add(ct.ComponentID,i::il)
    let private componentDictionary_AddEntity (cd:Map<Byte,uint32 list>) i (ctl:ComponentType list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.ContainsKey(ct.ComponentID) with
            | false -> newcd <- newcd.Add(ct.ComponentID,[i])
            | true -> let il = newcd.Item(ct.ComponentID)
                      newcd <- newcd.Remove(ct.ComponentID).Add(ct.ComponentID,i::il)
        newcd
    let private componentDictionary_RemoveComponent (cd:Map<Byte,uint32 list>) i (ct:ComponentType) =
        match cd.Item(ct.ComponentID) |> List.exists (fun x -> x=i) with
        | false -> cd
        | true -> let il = cd.Item(ct.ComponentID) |> List.filter (fun x -> x<>i)
                  cd.Remove(ct.ComponentID).Add(ct.ComponentID,il)
    let private componentDictionary_RemoveEntity (cd:Map<Byte,uint32 list>) i (ctl:ComponentType list) =
        let mutable newcd = cd
        for ct in ctl do
            match newcd.Item(ct.ComponentID) |> List.exists (fun x -> x=i) with
            | true -> let il = newcd.Item(ct.ComponentID) |> List.filter (fun x -> x<>i)
                      newcd <- newcd.Remove(ct.ComponentID).Add(ct.ComponentID,il)
            | false -> ()
        newcd
    let private tryGetComponent cid (ctl:ComponentType list) = 
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

    //Entity.ReplaceComponent ecd c.EntityID ComponentID_Form newc

    let TryGet ecd e =
        ecd.Entities.ContainsKey(e) |> TrueSomeFalseNone (ecd.Entities.Item(e))

    let TryGetComponent ecd cid e = 
        e |> TryGet ecd |> Option.bind (tryGetComponent cid)

