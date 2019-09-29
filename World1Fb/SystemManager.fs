module SystemManager
open CommonGenericFunctions
open Components
open EntityComponentManager

type ChangesAndNewECData = EntityComponentChange list * EntityComponentData

[<AbstractClass>]
type AbstractSystem(isActive:bool, requireInitialize:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.RequireInitialize = requireInitialize
    member this.IsInitialized = (_isInitialized || not requireInitialize)

    member internal this.SetToInitialized = _isInitialized <- true
    abstract member Initialize: EntityComponentChange list
    abstract member Update: EntityComponentData -> EntityComponentChange list

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
