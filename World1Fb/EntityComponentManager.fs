module EntityComponentManager
open Components

type EntityComponentData = {
    Entities : Map<uint32,EntityComponent list>
    MaxEntityID : uint32
    }
    
module Entity =
    let AllWithComponent ecd cid =
        ecd.Entities |> Map.filter (fun k ctl -> ctl |> List.exists (fun ct -> ct.ComponentID=cid))

    let Create ecd ctl =
        let i = ecd.MaxEntityID + 1u
        let ecl = ctl |> List.collect (fun ct -> [{ EntityID=i; Component=ct }])
        { Entities=ecd.Entities.Add(i,ecl); MaxEntityID=i }

    let GetComponent ecd cid e = 
        (ecd.Entities.Item(e) |> List.find (fun x -> x.ComponentID=cid)).Component

    let Remove ecd e = { Entities=ecd.Entities.Remove(e); MaxEntityID=ecd.MaxEntityID }

    let TryGet ecd e =
        match ecd.Entities.ContainsKey(e) with
        | false -> None
        | true -> Some (ecd.Entities.Item(e))

    let TryGetComponent ecd cid e = 
        match ecd.Entities.ContainsKey(e) with
        | false -> None
        | true -> let l = ecd.Entities.Item(e) |> List.filter (fun c -> c.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

