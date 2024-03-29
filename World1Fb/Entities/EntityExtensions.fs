﻿module EntityExtensions
open EntityManager
open CommonGenericFunctions
open Component
open ComponentEnums
open LocationTypes

module rec EntityExt = 

    let CopyEntity (enm:EntityManager) (round:RoundNumber) (oldeid:EntityID) =
        let neweid = enm.NewEntityID()
        oldeid
        |> enm.GetComponents
        |> Array.map (Component.Copy neweid enm.NewComponentID round)

    let FormImpassableAtLocation (enm:EntityManager) (excludeEID:EntityID option) (location:LocationDataInt) =
        location
        |> GetEntitiesAtLocationWithComponent enm FormComponent excludeEID
        |> Array.exists (fun (Form f) -> not f.IsPassable)

    let GetComponentForEntities (enm:EntityManager) (ct:ComponentType) (eids:EntityID[]) = 
        ct
        |> enm.GetComponentsOfType 
        |> Array.filter (fun (c:Component) -> eids |> Array.contains (GetComponentEntityID c))

    let GetComponentTypes (enm:EntityManager) (eid:EntityID) =
        eid
        |> enm.GetComponents
        |> Array.map GetComponentType

    let GetEntitiesAtLocationWithComponent (enm:EntityManager) (ct:ComponentType) (excludeEID:EntityID option) (location:LocationDataInt) = 
        location
        |> enm.GetEntityIDsAtLocation 
        |> Array.filter (fun eid -> excludeEID.IsNone || eid <> excludeEID.Value) // Not excluded or not me
        |> Array.choose (TryGetComponent enm ct)
    
    let GetLocation (enm:EntityManager) (eid:EntityID) = (ToForm (enm.GetComponent FormComponent eid)).Location

    let TryGetComponent (enm:EntityManager) (ct:ComponentType) (eid:EntityID) : Option<Component> = 
        match (enm.GetComponents eid) |> Array.filter (fun c -> GetComponentType c = ct) with
        | [||] -> None
        | cts -> Some cts.[0]




        //let GetHistory_Entities (entities:EntityManager) (round:uint32 option) = 
        //    let e,_,_ = entities.GetHistory round
        //    e

    //let GetEntitiesAtLocationWithComponents (entities:EntityManager) (round:uint32 option) (location:LocationDataInt) = 
    //    location
    //    |> entities.GetEntitiesAtLocation round
    //    |> Array.Parallel.map (fun eid -> entities.GetComponents round eid)

    //let GetEntitiesWithComponent (enm:EntityManager) (round:RoundNumber option) (ctid:ComponentTypeID) =
    //    ctid
    //    |> enm.GetComponentIDsByType round
    //    |> enm.GetComponents_ByID round


    //let GetComponent2 (entities:Map<uint32,Component[]>) (componentID:byte) (entityID:uint32) =
    //    entities.Item(entityID)
    //    |> Array.find (fun x -> x.ComponentID = componentID)

    //let GetHistory_Components (entities:EntityManager) (round:uint32 option) (componentID:byte) = 
    //    let _,components,_ = entities.GetHistory round
    //    match components.ContainsKey componentID with
    //    | false -> [||]
    //    | true -> components.Item componentID

    //let GetHistory_Locations (entities:EntityManager) (round:uint32 option) (location:LocationDataInt) =
    //    let _,_,locations = entities.GetHistory round
    //    match locations.ContainsKey location with
    //    | false -> [||]
    //    | true -> locations.Item location