module SystemManager
open AbstractComponent
open EntityManager
open EventManager
open GameEvents

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false
    let mutable _systemChangeLog = SystemChangeLog.empty

    member _.IsActive = isActive
    member _.IsInitialized = _isInitialized

    member internal this.ChangeLog_AddComponentChange c = _systemChangeLog <- _systemChangeLog.Add_ComponentChange c
    member internal this.ChangeLog_NewEntity ne = _systemChangeLog <- _systemChangeLog.Add_NewEntity ne
    member internal this.ChangeLog_PackageAndClose =
        let scl = _systemChangeLog
        _systemChangeLog <- SystemChangeLog.empty
        scl
    member internal this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    abstract member Update : SystemChangeLog


type SystemManager(evm:EventManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member private this.ConsolidateChangeLogs =
        this.ActiveAndInitialized 
        |> Array.map (fun x -> x.Update) 
        |> Array.fold (fun scl c -> c.Append scl) SystemChangeLog.empty

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems =
        this.ConsolidateChangeLogs

