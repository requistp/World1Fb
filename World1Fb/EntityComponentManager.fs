module EntityComponentManager
open Components
//open LocationTypes

type EntityComponentManager(entityComponents:Map<uint32,EntityComponent list>) =
    static let mutable _maxEntityID = uint32 0
    let mutable _componentDict = Map.empty:Map<int,uint32 list>

    let addToComponentDict (c:ComponentType) e =
        match _componentDict.ContainsKey(c.ComponentID) with
        | false -> _componentDict <- _componentDict.Add(c.ComponentID,[e])
        | true -> let l = [e] @ _componentDict.Item(c.ComponentID)
                  _componentDict <- _componentDict.Remove(c.ComponentID).Add(c.ComponentID,l)
    let tryGetEntity e =
        match entityComponents.ContainsKey(e) with
        | false -> None
        | true -> Some (entityComponents.Item(e))
                  
    do
        for e in entityComponents do
            if (e.Key > _maxEntityID) then _maxEntityID <- e.Key
            for v in e.Value do
                addToComponentDict v.Component v.EntityID

    member this.EntitiesWithComponent cid = 
        match _componentDict.ContainsKey(cid) with
        | true -> _componentDict.Item(cid)
        | false -> []
    member this.TryGetEntity e = tryGetEntity e
    member this.TryGetEntityComponent cid e = 
        match entityComponents.ContainsKey(e) with
        | false -> None
        | true -> let l = entityComponents.Item(e) |> List.filter (fun c -> c.Component.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

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