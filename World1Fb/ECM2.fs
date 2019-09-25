module ECM2
open Components

type EntityComponentManager(ecmap:Map<uint32,EntityComponent list>, compDict:Map<byte,uint32 list>, maxEntityID:uint32) = 
    let tryGetEntity e =
        match ecmap.ContainsKey(e) with
        | false -> None
        | true -> Some (ecmap.Item(e))
           
    member this.CreateEntity (ctl:ComponentType list) =
        let i = maxEntityID + 1u
        let mutable ecl = List.empty:EntityComponent list
        let mutable cd = compDict
        for ct in ctl do
            ecl <- [EntityComponent(i,ct)] @ ecl
            match cd.ContainsKey(ct.ComponentID) with
            | false -> cd <- cd.Add(ct.ComponentID,[i])
            | true -> let l = [i] @ cd.Item(ct.ComponentID)
                      cd <- cd.Remove(ct.ComponentID).Add(ct.ComponentID,l)        
        EntityComponentManager(ecmap.Add(i,ecl), cd, i)

    member this.CreateEntities_ViaComponentTypeList (ctl:ComponentType list) =
        let mutable ecm = this
        for ct in ctl do
            ecm <- ecm.CreateEntity([ct])
        ecm

    member this.ECMap = ecmap
    member this.EntitiesWithComponent cid = 
        match compDict.ContainsKey(cid) with
        | true -> compDict.Item(cid)
        | false -> []
    member this.GetEntityComponent cid e = 
        let ec = ecmap.Item(e) |> List.find (fun x -> x.Component.ComponentID=cid)
        ec.Component
    member this.TryGetEntity e = tryGetEntity e
    member this.TryGetEntityComponent cid e = 
        match ecmap.ContainsKey(e) with
        | false -> None
        | true -> let l = ecmap.Item(e) |> List.filter (fun c -> c.Component.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

    new() = EntityComponentManager(Map.empty, Map.empty, 0u)
