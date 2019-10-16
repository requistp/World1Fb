﻿module SystemManager
open EntityDictionary
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

    member this.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member this.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member this.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        this.Active |> Array.iter (fun s -> s.Initialize)
        evm.RegisterListener CreateEntity this.onCreateEntity
                
    member this.UpdateSystems =
        this.ActiveAndInitialized |> Array.Parallel.iter (fun s -> s.Update)

    member private this.onCreateEntity (next:NextEntityDictionary) (ge:AbstractGameEvent) =
        let e = (ge :?> Event_CreateEntity)
        next.CreateEntity e.Components

