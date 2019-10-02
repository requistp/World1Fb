module EntityComponentManager
open Components
open System

//type EntityComponentData = {
//    Entities : Map<uint32,EntityComponent list>
//    MaxEntityID : uint32
//    }
   
//Map<uint32,EntityComponent list>


module Entity =
    let AllWithComponent (ecd:Map<Guid,EntityComponent list>) cid =
        ecd |> Map.filter (fun k ctl -> ctl |> List.exists (fun ct -> ct.ComponentID=cid))

    let GetComponent (ecd:Map<Guid,EntityComponent list>) cid e = 
        (ecd.Item(e) |> List.find (fun x -> x.ComponentID=cid)).Component

    let TryGet (ecd:Map<Guid,EntityComponent list>) e =
        match ecd.ContainsKey(e) with
        | false -> None
        | true -> Some (ecd.Item(e))

    let TryGetComponent (ecd:Map<Guid,EntityComponent list>) cid e = 
        match ecd.ContainsKey(e) with
        | false -> None
        | true -> let l = ecd.Item(e) |> List.filter (fun c -> c.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

