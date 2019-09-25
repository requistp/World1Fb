module EntityComponentManager
open Components

let maintainComponentDict_Add (cd:Map<byte,uint32 list>) (ec:EntityComponent) =
    match cd.ContainsKey(ec.Component.ComponentID) with
    | false -> cd.Add(ec.Component.ComponentID,[ec.EntityID])
    | true -> let l = [ec.EntityID] @ cd.Item(ec.Component.ComponentID)
              cd.Remove(ec.Component.ComponentID).Add(ec.Component.ComponentID,l)
   
type EntityComponentManager(ecmap:Map<uint32,EntityComponent list>, maxEntityID:uint32, compDict:Map<byte,uint32 list>) = 
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



////type EntityComponentManager(entities:Set<int>, entityComponents:Map<int,AbstractComponent list>, maxEntityID:int) =
////    member this.Entities = entities
////    member this.EntitiesWithComponent (ct:System.Type) = Map.filter (fun k v -> ComponentExists ct v) entityComponents
////    member this.EntityIDsWithComponent (ct:System.Type) = this.EntitiesWithComponent ct |> Map.toSeq |> Seq.map fst |> Set.ofSeq
////    member this.GetComponent (ct:System.Type) e = entityComponents.Item e |> List.find (fun x -> x.GetType() = ct)
////    member this.MaxEntityID = maxEntityID
////    member this.TryGetComponent (ct:System.Type) e = entityComponents.Item e |> TryGetComponent ct

////    member this.CreateEntity (comps:AbstractComponent list) = 
////        let id = maxEntityID + 1
////        new EntityComponentManager(entities.Add(id), entityComponents.Add(id,comps), id)
          
 

////let e = EntityID 0
////let e2 = EntityID 1
////let tc = TerrainComponent(Dirt, LocationDataInt(0,0))
////let c1 = Terrain tc
////let ec1 = EntityComponent(e,c1)
////let ec12 = EntityComponent(e2,c1)
 
////let fc = FormComponent(true, "junk", ';', LocationDataFloat(0.0,0.0))
////let c2 = Form fc
////let ec2 = EntityComponent(e,c2)
////let ec22 = EntityComponent(e2,c2)
 
////let ecl = [ec1] @ [ec2]
////let ecl2 = [ec22] //@ [ec22]
 
////let m1 = Map.empty.Add(e,ecl).Add(e2,ecl2)
 
////let ecm = new EntityComponentManager(m1)
 
////printfn "compDict=%O" ecm.ComponentDict


//member this.Add (ecl:EntityComponent list) = 
//    let mutable cd = compDict
//    for v in ecl do
//        match cd.ContainsKey(v.Component.ComponentID) with
//        | false -> cd <- cd.Add(v.Component.ComponentID,[ecl.Head.EntityID])
//        | true -> let l = [ecl.Head.EntityID] @ cd.Item(v.Component.ComponentID)
//                  cd <- cd.Remove(v.Component.ComponentID).Add(v.Component.ComponentID,l)
//    EntityComponentData(ecmap.Add(maxEntityID+1u,ecl), maxEntityID+1u, cd)