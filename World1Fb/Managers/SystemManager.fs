module SystemManager


[<AbstractClass>]
type AbstractSystem(isActive:bool) =
    let mutable _isInitialized = false

    member _.IsActive = isActive
    member _.IsInitialized = _isInitialized
    member _.SetToInitialized = _isInitialized <- true
    
    abstract member ToString : string

    abstract member Initialize : unit
    default me.Initialize = me.SetToInitialized

    abstract member Update : uint32 -> unit

    member me.Abstract = me :> AbstractSystem


type SystemManager() =
    let mutable _systems = Array.empty<AbstractSystem>

    member _.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member _.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)

    member me.Init (ss:AbstractSystem[]) =
        _systems <- ss
        me.Active |> Array.Parallel.iter (fun s -> s.Initialize)

    member me.Init (s:string[]) =
        ()

    member me.UpdateSystems round =
        me.ActiveAndInitialized |> Array.Parallel.iter (fun s -> s.Update round)


