module SystemManager
open AbstractComponent
open EventManager


type ChangeLogManager() =
    let mutable _systemChangeLog = SystemChangeLog.empty
    static member AppendLogs (scl1:SystemChangeLog) (scl2:SystemChangeLog) = 
        {
            ComponentChanges = Array.append scl1.ComponentChanges scl2.ComponentChanges
            NewEntities = Array.append scl1.NewEntities scl2.NewEntities
        }
    member this.AddComponentChange (c:AbstractComponentChange) =
        _systemChangeLog <- {_systemChangeLog with ComponentChanges = Array.append _systemChangeLog.ComponentChanges [|c|] }
    member this.NewEntity (ne:AbstractComponent[]) = 
        _systemChangeLog <- { _systemChangeLog with NewEntities = Array.append _systemChangeLog.NewEntities [|ne|] }
    member this.PackageAndClose =
        let scl = _systemChangeLog
        _systemChangeLog <- SystemChangeLog.empty
        scl


[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    let clm = new ChangeLogManager()

    member this.ChangeLog = clm
    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized
    member this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    abstract member Update : SystemChangeLog


type SystemManager(evm:EventManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member private this.ConsolidateChangeLogs =
        this.ActiveAndInitialized 
        |> Array.map (fun s -> s.Update) // Parallel seems risky as I don't know what that updates might do
        |> Array.fold (fun scl c -> ChangeLogManager.AppendLogs scl c) SystemChangeLog.empty

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems =
        this.ConsolidateChangeLogs

