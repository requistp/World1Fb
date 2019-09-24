module EntityComponentManager
open Components

//[<Struct>]
//type EntityComponentData(ecmap:Map<uint32,EntityComponent list>, maxEntityID:uint32) =
//    static member New = EntityComponentData(Map.empty, 0u)
//    member this.MaxEntityID = maxEntityID
//    member this.Map = ecmap
//    member this.Add (ecl:EntityComponent list) = EntityComponentData(ecmap.Add(maxEntityID+1u,ecl), maxEntityID+1u)

[<Struct>]
type EntityComponentData(ecmap:Map<uint32,EntityComponent list>, maxEntityID:uint32, compDict:Map<int,uint32 list>) =
    static member New = EntityComponentData(Map.empty, 0u, Map.empty)
    member this.MaxEntityID = maxEntityID
    member this.Map = ecmap
    member this.CompDict = compDict
    member this.Add (ecl:EntityComponent list) = 
        let mutable cd = compDict
        for v in ecl do
            match cd.ContainsKey(v.Component.ComponentID) with
            | false -> cd <- cd.Add(v.Component.ComponentID,[ecl.Head.EntityID])
            | true -> let l = [ecl.Head.EntityID] @ cd.Item(v.Component.ComponentID)
                      cd <- cd.Remove(v.Component.ComponentID).Add(v.Component.ComponentID,l)
        EntityComponentData(ecmap.Add(maxEntityID+1u,ecl), maxEntityID+1u, cd)

//let BuildComponentDict (ecmap:Map<uint32,EntityComponent list>) = 
//    let mutable _componentDict = Map.empty:Map<int,uint32 list>
//    let st = System.DateTime.Now
//    printfn "building count:%i" ecmap.Count
//    for e in ecmap do
//        for v in e.Value do
//            match _componentDict.ContainsKey(v.Component.ComponentID) with
//            | false -> _componentDict <- _componentDict.Add(v.Component.ComponentID,[e.Key])
//            | true -> let l = [e.Key] @ _componentDict.Item(v.Component.ComponentID)
//                      _componentDict <- _componentDict.Remove(v.Component.ComponentID).Add(v.Component.ComponentID,l)
//    let et = System.DateTime.Now
//    printfn "\nBuildComponentDict start:%O, %i" st st.Millisecond
//    printfn "\nBuildComponentDict   end:%O, %i" et et.Millisecond
//    _componentDict

type EntityComponentManager(ecmap:EntityComponentData) = 
    //let _componentDict = BuildComponentDict ecmap.Map

    let tryGetEntity e =
        match ecmap.Map.ContainsKey(e) with
        | false -> None
        | true -> Some (ecmap.Map.Item(e))
           
    member this.ECMap = ecmap
    member this.EntitiesWithComponent cid = 
        match ecmap.CompDict.ContainsKey(cid) with
        | true -> ecmap.CompDict.Item(cid)
        | false -> []
    member this.GetEntityComponent cid e = 
        let ec = ecmap.Map.Item(e) |> List.find (fun x -> x.Component.ComponentID=cid)
        ec.Component
    member this.TryGetEntity e = tryGetEntity e
    member this.TryGetEntityComponent cid e = 
        match ecmap.Map.ContainsKey(e) with
        | false -> None
        | true -> let l = ecmap.Map.Item(e) |> List.filter (fun c -> c.Component.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

    new() = EntityComponentManager(EntityComponentData.New)



//type EntityComponentManager(entities:Set<int>, entityComponents:Map<int,AbstractComponent list>, maxEntityID:int) =
//    member this.Entities = entities
//    member this.EntitiesWithComponent (ct:System.Type) = Map.filter (fun k v -> ComponentExists ct v) entityComponents
//    member this.EntityIDsWithComponent (ct:System.Type) = this.EntitiesWithComponent ct |> Map.toSeq |> Seq.map fst |> Set.ofSeq
//    member this.GetComponent (ct:System.Type) e = entityComponents.Item e |> List.find (fun x -> x.GetType() = ct)
//    member this.MaxEntityID = maxEntityID
//    member this.TryGetComponent (ct:System.Type) e = entityComponents.Item e |> TryGetComponent ct

//    member this.CreateEntity (comps:AbstractComponent list) = 
//        let id = maxEntityID + 1
//        new EntityComponentManager(entities.Add(id), entityComponents.Add(id,comps), id)
          
 

//let e = EntityID 0
//let e2 = EntityID 1
//let tc = TerrainComponent(Dirt, LocationDataInt(0,0))
//let c1 = Terrain tc
//let ec1 = EntityComponent(e,c1)
//let ec12 = EntityComponent(e2,c1)
 
//let fc = FormComponent(true, "junk", ';', LocationDataFloat(0.0,0.0))
//let c2 = Form fc
//let ec2 = EntityComponent(e,c2)
//let ec22 = EntityComponent(e2,c2)
 
//let ecl = [ec1] @ [ec2]
//let ecl2 = [ec22] //@ [ec22]
 
//let m1 = Map.empty.Add(e,ecl).Add(e2,ecl2)
 
//let ecm = new EntityComponentManager(m1)
 
//printfn "compDict=%O" ecm.ComponentDict