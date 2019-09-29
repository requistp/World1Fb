module EntityComponentManager
open Components

//type EntityComponentData(ecm:Map<uint32,EntityComponent list>, maxEntityID:uint32) =
//    static member New = EntityComponentData(Map.empty, 0u)
//    member this.ECMap = ecm
//    member this.MaxEntityID = maxEntityID

type EntityComponentData = {
    ECMap : Map<uint32,EntityComponent list>
    MaxEntityID : uint32
    }

module Entity =
    let AllWithComponent (ecd:EntityComponentData) (cid:byte) =
        ecd.ECMap |> Map.filter (fun k ctl -> ctl |> List.exists (fun ct -> ct.Component.ComponentID=cid))

    let Create (ecd:EntityComponentData) (ctl:ComponentType list) =
        let i = ecd.MaxEntityID + 1u
        let ecl = ctl |> List.collect (fun ct -> [EntityComponent(i,ct)])
        { ECMap = ecd.ECMap.Add(i,ecl); MaxEntityID = i }

    let GetComponent (ecd:EntityComponentData) cid e = 
        (ecd.ECMap.Item(e) |> List.find (fun x -> x.Component.ComponentID=cid)).Component

    let Remove (ecd:EntityComponentData) e = //EntityComponentData(ecd.ECMap.Remove(e), ecd.MaxEntityID)
        { ECMap = ecd.ECMap.Remove(e); MaxEntityID = ecd.MaxEntityID }

    let TryGet (ecd:EntityComponentData) e =
        match ecd.ECMap.ContainsKey(e) with
        | false -> None
        | true -> Some (ecd.ECMap.Item(e))

    let TryGetComponent (ecd:EntityComponentData) cid e = 
        match ecd.ECMap.ContainsKey(e) with
        | false -> None
        | true -> let l = ecd.ECMap.Item(e) |> List.filter (fun c -> c.Component.ComponentID=cid)
                  match l with
                  | [] -> None
                  | _ -> Some l.Head

