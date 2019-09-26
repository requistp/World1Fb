module SystemManager
open CommonGenericFunctions
open EntityComponentManager

[<AbstractClass>]
type AbstractSystem(isActive:bool, requireInitialize:bool) =
    let mutable _isInitialized = false

    member this.IsActive = isActive
    member this.RequireInitialize = requireInitialize
    member this.IsInitialized
        with get () = (_isInitialized || not requireInitialize)
        and internal set (value) = _isInitialized <- value

    abstract member Initialize: EntityComponentManager -> EntityComponentManager
    abstract member Update: float -> EntityComponentManager -> EntityComponentManager

type SystemManager() =
    let mutable _systems = List.empty:AbstractSystem list
    let mutable _lastUpdated = 0.0

    let registerAllSystems (sl:AbstractSystem list) (ecm:EntityComponentManager) = 
        _systems <- sl
        for s in sl do
            s.Initialize ecm
        Success "Systems registered"

    let updateAllSystems (ecm:EntityComponentManager) = 
        printfn "update all systems"
        ecm

    member this.IsInitialized = not _systems.IsEmpty
    
    member this.RegisterSystems (sl:AbstractSystem list) (ecm:EntityComponentManager) = 
        match sl.IsEmpty with
        | true -> Failure "No systems to register"
        | false -> registerAllSystems sl

    member this.Update (ecm:EntityComponentManager) = 
        let dt = _lastUpdated
        _lastUpdated <- 0.0
        let newecm = updateAllSystems ecm 
        Success (newecm, dt)
