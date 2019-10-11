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
//    let private eventQueueing (eid:uint32) (ctl:AbstractComponent[]) =
  //      if ctl |> Array.exists (fun ct -> ct.ComponentType = Terrain)

    let AllWithComponent ecd componentType =
        match ecd.Components.ContainsKey(componentType) with
        | true -> ecd.Components.Item(componentType)
        | false -> Array.empty

    let Create ecd ctl =
        let eid = ecd.MaxEntityID + 1u
    //    eventQueueing eid ctl
        {
            Entities = ecd.Entities.Add(eid,ctl)
            Components = componentDictionary_AddEntity ecd.Components eid ctl
            MaxEntityID = eid
        }

    let EmptyECD = 
        {
            Entities = Map.empty
            Components = Map.empty
            MaxEntityID = 0u 
        }

    let GetComponent ecd componentType eid = 
        ecd.Entities.Item(eid) |> Array.find (fun x -> x.ComponentType=componentType)

    let Remove ecd eid = 
        {
            Entities = ecd.Entities.Remove(eid)
            Components = componentDictionary_RemoveEntity ecd.Components eid (ecd.Entities.Item(eid))
            MaxEntityID = ecd.MaxEntityID
        }

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

