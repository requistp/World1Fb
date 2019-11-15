module EntityManager
open agent_Entities
open CommonGenericFunctions
open Component
open ComponentEnums
open LocationTypes

module rec Entities = 

    let CopyEntity (entities:agent_Entities) (oldeid:uint32) =
        let neweid = entities.GetNewID
        oldeid
        |> entities.GetComponents
        |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    let GetComponent2 (entities:Map<uint32,Component[]>) (componentID:byte) (entityID:uint32) =
        entities.Item(entityID)
        |> Array.find (fun x -> x.ComponentID = componentID)

    let GetComponentForEntities (entities:agent_Entities) (componentID:byte) (entityIDs:uint32[]) = 
        componentID
        |> entities.GetEntitiesWithComponent
        |> Array.filter (fun e -> entityIDs |> Array.contains e)
        |> Array.Parallel.map (fun e -> entities.GetComponent componentID e)

    let GetComponentIDs (entities:agent_Entities) (eid:uint32) =
        eid 
        |> entities.GetComponents 
        |> Array.Parallel.map (fun ct -> ct.ComponentID)

    let GetEntitiesAtLocationWithComponent (entities:agent_Entities) (componentID:byte) (excludeEntityID:uint32 option) (location:LocationDataInt) = 
        location
        |> entities.GetEntitiesAtLocation
        |> Array.filter (fun eid -> excludeEntityID.IsNone || eid <> excludeEntityID.Value) // Not excluded or not me
        |> Array.Parallel.choose (fun eid -> eid |> TryGetComponent entities componentID)

    let GetHistory_Components (entities:agent_Entities) (round:uint32 option) (componentID:byte) = 
        let _,components,_ = entities.GetHistory round
        match components.ContainsKey componentID with
        | false -> [||]
        | true -> components.Item componentID
    
    let GetHistory_Entities (entities:agent_Entities) (round:uint32 option) = 
        let e,_,_ = entities.GetHistory round
        e

    let GetHistory_Locations (entities:agent_Entities) (round:uint32 option) (location:LocationDataInt) =
        let _,_,locations = entities.GetHistory round
        match locations.ContainsKey location with
        | false -> [||]
        | true -> locations.Item location

    let GetLocation (entities:agent_Entities) (entityID:uint32) = 
        (entityID |> entities.GetComponent FormComponentID).ToForm.Location

    let TryGet (entities:agent_Entities) (entityID:uint32) =
        entityID 
        |> entities.EntityExists 
        |> TrueSomeFalseNone (entities.GetComponents entityID)

    let TryGetComponent (entities:agent_Entities) (componentID:byte) (entityID:uint32) : Option<Component> = 
        entityID
        |> TryGet entities
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentID = componentID) with
            | [||] -> None
            | l -> Some (l.[0]) )



