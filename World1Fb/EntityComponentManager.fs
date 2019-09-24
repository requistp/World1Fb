module EntityComponentManager
open Components

[<Struct>]
type EntityComponentMap(ecmap:Map<uint32,EntityComponent list>, maxEntityID:uint32) =
    static member New = EntityComponentMap(Map.empty, 0u)
    member this.MaxEntityID = maxEntityID
    member this.Map = ecmap
    member this.Add (ecl:EntityComponent list) = EntityComponentMap(ecmap.Add(maxEntityID+1u,ecl), maxEntityID+1u)

let BuildComponentDict (ecmap:Map<uint32,EntityComponent list>) = 
    let mutable _componentDict = Map.empty:Map<int,uint32 list>
    for e in ecmap do
        for v in e.Value do
            match _componentDict.ContainsKey(v.Component.ComponentID) with
            | false -> _componentDict <- _componentDict.Add(v.Component.ComponentID,[e.Key])
            | true -> let l = [e.Key] @ _componentDict.Item(v.Component.ComponentID)
                      _componentDict <- _componentDict.Remove(v.Component.ComponentID).Add(v.Component.ComponentID,l)
    _componentDict

[<Struct>]
type ComponentDict(ecmap:Map<uint32,EntityComponent list>) =
    member this.CD = BuildComponentDict ecmap

type EntityComponentManager(ecmap:EntityComponentMap) = 
    let mutable _componentDict = Map.empty:Map<int,uint32 list>

    let addToComponentDict cid e =
        match _componentDict.ContainsKey(cid) with
        | false -> _componentDict <- _componentDict.Add(cid,[e])
        | true -> let l = [e] @ _componentDict.Item(cid)
                  _componentDict <- _componentDict.Remove(cid).Add(cid,l)
    let tryGetEntity e =
        match ecmap.Map.ContainsKey(e) with
        | false -> None
        | true -> Some (ecmap.Map.Item(e))
           
    do
        for e in ecmap.Map do
            for v in e.Value do
                addToComponentDict v.Component.ComponentID v.EntityID

    member this.ECMap = ecmap
    member this.EntitiesWithComponent cid = 
        match _componentDict.ContainsKey(cid) with
        | true -> _componentDict.Item(cid)
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

    new() = EntityComponentManager(EntityComponentMap.New)

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