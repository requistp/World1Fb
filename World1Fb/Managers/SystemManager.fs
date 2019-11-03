module SystemManager
open AbstractSystem
open EntityManager
open EventManager
open EventTypes
    

type SystemManager(enm:EntityManager,evm:EventManager) =
    let mutable _systems = Array.empty<AbstractSystem>

    member me.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member me.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)
    
    member private me.onCreateEntity (ge:GameEventTypes) =
        enm.CreateEntity (ge.ToCreateEntity.Components)

    member me.Initialize (ss:AbstractSystem[]) =
        _systems <- ss
        me.Active |> Array.Parallel.iter (fun s -> s.Initialize)
        evm.RegisterListener "SystemManager" Event_CreateEntity.ID me.onCreateEntity
                
    member me.UpdateSystems =
        me.ActiveAndInitialized 
        |> Array.Parallel.iter (fun s -> s.Update)


