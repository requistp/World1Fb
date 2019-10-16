module SystemManager
open AbstractComponent
open EventManager
open GameEvents

[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.IsInitialized = _isInitialized
    member this.SetToInitialized = _isInitialized <- true

    abstract member Initialize : unit
    
    abstract member Update : unit
    

type SystemManager(evm:EventManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member private this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member private this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)

    member this.UpdateSystems =
        this.ActiveAndInitialized |> Array.Parallel.iter (fun s -> s.Update)



(*
type ChangeLogManager() =
    let mutable _systemChangeLog = SystemChangeLog.empty
    static member AppendLogs (scl1:SystemChangeLog) (scl2:SystemChangeLog) = 
        {
            ComponentChanges = Array.append scl1.ComponentChanges scl2.ComponentChanges
            ChangeResults = Array.append scl1.ChangeResults scl2.ChangeResults
            NewEntities = Array.append scl1.NewEntities scl2.NewEntities
        }
    member this.AddComponentChange (c:AbstractComponentChange) =
        _systemChangeLog <- {_systemChangeLog with ComponentChanges = Array.append _systemChangeLog.ComponentChanges [|c|] }
    member this.NewEntities (nes:AbstractComponent[][]) = 
        _systemChangeLog <- { _systemChangeLog with NewEntities = Array.append _systemChangeLog.NewEntities nes }
    member this.PackageAndClose =
        let scl = _systemChangeLog
        _systemChangeLog <- SystemChangeLog.empty
        scl
*)