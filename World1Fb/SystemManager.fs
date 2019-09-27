module SystemManager
open ChangeLogManager
open CommonGenericFunctions
open EntityComponentManager
open System_Abstract

type SystemManager() =
    let mutable _systems = List.empty:AbstractSystem list
    let mutable _lastUpdated = 0.0
    let clm = ChangeLogManager()

    let applyChangeLog = 
        1

    let registerAllSystems (sl:AbstractSystem list) = 
        match sl.IsEmpty with
        | true -> Error "Systems list is empty"
        | false -> _systems <- sl
                   let rl = sl |> List.collect (fun s -> [s.Initialize clm])
                   //I should check if an error exists in rl, but I don't know how yet
                   Ok 1

    let updateAllSystems (ecm:EntityComponentManager) = 
        printfn "update all systems"
        ecm

    member this.ChangeLogManager = clm
    member this.IsInitialized = (not _systems.IsEmpty) && (not (_systems |> List.exists (fun s -> not s.IsInitialized)))
    member this.RegisterSystems (sl:AbstractSystem list) = registerAllSystems sl

    member this.Update (ecm:EntityComponentManager) = 
        let newecm = updateAllSystems ecm 
        Ok newecm
