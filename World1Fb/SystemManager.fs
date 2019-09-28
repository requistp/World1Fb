module SystemManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract

type ChangesAndNewECM = EntityComponentChange list * EntityComponentManager    

type SystemManager() =
    let mutable _systems = List.empty:AbstractSystem list

    let applyChangeLog (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = 
        let mutable newecm = ecm
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecm <- newecm.CreateEntity ctl
            | EntityRemoval e -> newecm <- newecm.RemoveEntity e
            | _ -> () //newecm
        Ok (ChangesAndNewECM (eccl,newecm))
    let collectAndApplyChanges fx ecm (asl:AbstractSystem list) =
        asl
        |> List.collect fx
        |> applyChangeLog ecm

    member this.IsInitialized = (not _systems.IsEmpty) && not (_systems |> List.exists (fun s -> s.IsActive && not s.IsInitialized))
    member this.RegisterSystems (sl:AbstractSystem list) = 
        match sl.IsEmpty with
        | true -> Error "Systems list is empty"
        | false -> _systems <- sl
                   _systems 
                   |> List.filter (fun x -> x.IsActive)
                   |> collectAndApplyChanges (fun s -> s.Initialize) (EntityComponentManager.New)
    member this.Update ecm = 
        match this.IsInitialized with
        | false -> Error "Systems not initialized"
        | true -> _systems 
                  |> List.filter (fun x -> x.IsActive && x.IsInitialized)
                  |> collectAndApplyChanges (fun s -> s.Update ecm) ecm
