module EntityExtensions
open EntityManager
open CommonGenericFunctions
open Component
open ComponentEnums
open LocationTypes

module rec EntityExt = 

    let CopyEntity (enm:EntityManager) (oldeid:uint32) =
        let neweid = enm.NewEntityID()
        oldeid
        |> enm.GetComponents None
        |> Array.Parallel.map (Component.Copy neweid (enm.NewComponentID()))

    //let GetComponent2 (entities:Map<uint32,Component[]>) (componentID:byte) (entityID:uint32) =
    //    entities.Item(entityID)
    //    |> Array.find (fun x -> x.ComponentID = componentID)

    let GetComponentForEntities (enm:EntityManager) (round:RoundNumber option) (ctid:ComponentTypeID) (eids:EntityID[]) = 
        ctid
        |> GetEntitiesWithComponent enm round
        |> Array.filter (fun (c:Component) -> eids |> Array.contains c.EntityID)

    let GetComponentTypeIDs (enm:EntityManager) (round:RoundNumber option) (eid:EntityID) =
        eid
        |> enm.GetComponents round
        |> Array.Parallel.map Component.GetComponentTypeID

    let GetEntitiesAtLocationWithComponent (enm:EntityManager) (round:RoundNumber option) (ctid:ComponentTypeID) (excludeEID:EntityID option) (location:LocationDataInt) = 
        location
        |> enm.GetEntityIDsAtLocation round
        |> Array.filter (fun eid -> excludeEID.IsNone || eid <> excludeEID.Value) // Not excluded or not me
        |> Array.Parallel.choose (TryGetComponent enm round ctid)

    //let GetEntitiesAtLocationWithComponents (entities:EntityManager) (round:uint32 option) (location:LocationDataInt) = 
    //    location
    //    |> entities.GetEntitiesAtLocation round
    //    |> Array.Parallel.map (fun eid -> entities.GetComponents round eid)

    let GetEntitiesWithComponent (enm:EntityManager) (round:RoundNumber option) (ctid:ComponentTypeID) =
        ctid
        |> enm.GetComponentIDsByType round
        |> enm.GetComponents_ByID round

    let GetLocation (enm:EntityManager) (round:RoundNumber option) (eid:EntityID) = 
        (eid |> enm.GetComponentByType round FormComponentID).[0].ToForm.Location

    let TryGet (enm:EntityManager) (round:RoundNumber option) (eid:EntityID) =
        eid 
        |> enm.EntityExists round
        |> TrueSomeFalseNone (enm.GetComponents round eid)

    let TryGetComponent (enm:EntityManager) (round:RoundNumber option) (ctid:ComponentTypeID) (eid:EntityID) : Option<Component> = 
        eid
        |> TryGet enm round
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentTypeID = ctid) with
            | [||] -> None
            | l -> Some (l.[0]) )



    //let GetHistory_Components (entities:EntityManager) (round:uint32 option) (componentID:byte) = 
    //    let _,components,_ = entities.GetHistory round
    //    match components.ContainsKey componentID with
    //    | false -> [||]
    //    | true -> components.Item componentID
    //let GetHistory_Entities (entities:EntityManager) (round:uint32 option) = 
    //    let e,_,_ = entities.GetHistory round
    //    e
    //let GetHistory_Locations (entities:EntityManager) (round:uint32 option) (location:LocationDataInt) =
    //    let _,_,locations = entities.GetHistory round
    //    match locations.ContainsKey location with
    //    | false -> [||]
    //    | true -> locations.Item location