module SystemManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract

type ChangesAndNewECM = EntityComponentChange list * EntityComponentManager    

type SystemManager() =
    let mutable _systems = List.empty:AbstractSystem list

    let setSystems sl = _systems <- sl
    let applyChangeLog (ecm:EntityComponentManager) (eccl:EntityComponentChange list) = 
        let mutable newecm = ecm
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecm <- newecm.CreateEntity ctl
            | _ -> () //newecm
        Ok (ChangesAndNewECM (eccl,newecm))
    let executeSystems fx =
        _systems
        |> List.filter (fun x -> x.IsActive)
        |> List.collect (fun s -> fx s)

    let registerAllSystems (sl:AbstractSystem list) = 
        setSystems sl
        let eccl = _systems |> List.collect (fun s -> s.Initialize)
        let eccl2 = (executeSystems )
        //I should maybe check for uninitialized systems here
        applyChangeLog EntityComponentManager.New eccl

    let updateAllSystems (ecm:EntityComponentManager) = 
        printfn "update all systems"
        ecm

    member this.IsInitialized = (not _systems.IsEmpty) && (not (_systems |> List.exists (fun s -> not s.IsInitialized)))
    member this.RegisterSystems (sl:AbstractSystem list) = 
        match sl.IsEmpty with
        | true -> Error "Systems list is empty"
        | false -> registerAllSystems sl

    member this.Update (ecm:EntityComponentManager) = 
        let newecm = updateAllSystems ecm 
        Ok newecm
