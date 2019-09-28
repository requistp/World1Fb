module SystemManager
open CommonGenericFunctions
open Components
open EntityComponentManager
open System_Abstract

module SystemManager =

    let private applyChangeLog (ecd:EntityComponentData) (eccl:EntityComponentChange list) = 
        let mutable newecd = ecd
        for ecc in eccl do 
            match ecc with
            | EntityAddition ctl -> newecd <- Entity.Create newecd ctl
            | EntityRemoval e -> newecd <- Entity.Remove newecd e
            | _ -> () //newecm
        ChangesAndNewECData (eccl,newecd)

    let private collectAndApplyChange filterFx collectFx ecm (sl:AbstractSystem list) =
        sl
        |> List.filter filterFx
        |> List.collect collectFx
        |> applyChangeLog ecm

    let RegisterSystems (sl:AbstractSystem list) = 
        collectAndApplyChange (fun x -> x.IsActive) (fun s -> s.Initialize) (EntityComponentData.New) sl

    let Update (sl:AbstractSystem list) ecm = 
        collectAndApplyChange (fun x -> x.IsActive && x.IsInitialized) (fun s -> s.Update ecm) ecm sl
