module EntityComponentManager
open Components

type EntityComponentManager(ecmap:Map<uint32,EntityComponent list>, maxEntityID:uint32, compDict:Map<byte,uint32 list>) = 
    let maintainComponentDict_Add (cd:Map<byte,uint32 list>) (ec:EntityComponent) =
        match cd.ContainsKey(ec.Component.ComponentID) with
        | false -> cd.Add(ec.Component.ComponentID,[ec.EntityID])
        | true -> let l = [ec.EntityID] @ cd.Item(ec.Component.ComponentID)
                  cd.Remove(ec.Component.ComponentID).Add(ec.Component.ComponentID,l)
    let tryGetEntity e =
        match ecmap.ContainsKey(e) with
        | false -> None
        | true -> Some (ecmap.Item(e))

    member this.Entities = ecmap    
    member this.CreateEntity (ctl:ComponentType list) =
        let i = maxEntityID + 1u
        let mutable ecl = List.empty:EntityComponent list
        let mutable cd = compDict
        for ct in ctl do
            let ec = EntityComponent(i,ct)
            ecl <- [ec] @ ecl
            cd <- maintainComponentDict_Add cd ec
        EntityComponentManager(ecmap.Add(i,ecl), i, cd)
    member this.CreateEntities_ViaComponentTypeList (ctl:ComponentType list) =
        let mutable i = maxEntityID
        let mutable newmap = ecmap
        let mutable newcd = compDict

        for ct in ctl do
            i <- i + 1u
            let ec = EntityComponent(i,ct)
            newmap <- ecmap.Add(i,[ec])
            newcd <- maintainComponentDict_Add newcd ec
        EntityComponentManager(newmap, i, newcd)
    member this.EntitiesWithComponent (cid:byte) = 
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

    new() = EntityComponentManager(Map.empty, 0u, Map.empty)
