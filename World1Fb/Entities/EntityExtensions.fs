﻿module EntityExtensions
open EntityManager
open CommonGenericFunctions
open Component
open ComponentEnums
open LocationTypes

module rec EntityExt = 

    //let CopyEntity (entities:EntityManager) (oldeid:uint32) =
    //    let neweid = entities.GetNewID
    //    oldeid
    //    |> entities.GetComponents
    //    |> Array.Parallel.map (fun ct -> ct.Copy neweid)

    //let GetComponent2 (entities:Map<uint32,Component[]>) (componentID:byte) (entityID:uint32) =
    //    entities.Item(entityID)
    //    |> Array.find (fun x -> x.ComponentID = componentID)

    //let GetComponentForEntities (entities:EntityManager) (componentID:byte) (entityIDs:uint32[]) = 
    //    componentID
    //    |> entities.GetEntitiesWithComponent
    //    |> Array.filter (fun e -> entityIDs |> Array.contains e)
    //    |> Array.Parallel.map (entities.GetComponent componentID)

    //let GetComponentIDs (entities:EntityManager) (eid:uint32) =
    //    eid 
    //    |> entities.GetComponents 
    //    |> Array.Parallel.map (fun ct -> ct.ComponentID)

    let GetEntitiesAtLocationWithComponent (entities:EntityManager) (round:uint32 option) (componentID:byte) (excludeEntityID:uint32 option) (location:LocationDataInt) = 
        location
        |> entities.GetEntitiesAtLocation round
        |> Array.filter (fun eid -> excludeEntityID.IsNone || eid <> excludeEntityID.Value) // Not excluded or not me
        |> Array.Parallel.choose (TryGetComponent entities round componentID)

    let GetEntitiesAtLocationWithComponents (entities:EntityManager) (round:uint32 option) (location:LocationDataInt) = 
        location
        |> entities.GetEntitiesAtLocation round
        |> Array.Parallel.map (fun eid -> entities.GetComponents round eid)

    //let GetEntitiesWithComponent (entities:EntityManager) (componentID:byte) =
    //    componentID
    //    |> entities.GetEntitiesWithComponent
    //    |> Array.Parallel.map (entities.GetComponent componentID)

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

    //let GetLocation (entities:EntityManager) (entityID:uint32) = 
    //    (entityID |> entities.GetComponent FormComponentID).ToForm.Location

    let TryGet (entities:EntityManager) (round:uint32 option) (entityID:uint32) =
        entityID 
        |> entities.EntityExists round
        |> TrueSomeFalseNone (entities.GetComponents round entityID)

    let TryGetComponent (entities:EntityManager) (round:uint32 option) (componentID:byte) (entityID:uint32) : Option<Component> = 
        entityID
        |> TryGet entities round
        |> Option.bind (fun cts -> 
            match cts |> Array.filter (fun c -> c.ComponentID = componentID) with
            | [||] -> None
            | l -> Some (l.[0]) )


