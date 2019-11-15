module SystemManager
open agent_SystemWorkTracker
open EventTypes


[<AbstractClass>]
type AbstractSystem(description:string, isActive:bool) =
    let mutable _isInitialized = false
    let agentForWorkTracking = new agent_SystemWorkTracker()

    member _.Description = description
    member _.IsActive = isActive
    member _.IsIdle = agentForWorkTracking.IsIdle
    member _.IsInitialized = _isInitialized
    member _.SetToInitialized = _isInitialized <- true
    
    member _.TrackTask (task:uint32->GameEventTypes->Result<string option,string>) (round:uint32) (ge:GameEventTypes) = 
        agentForWorkTracking.Start
        let result = task round ge
        agentForWorkTracking.End
        result

    abstract member Initialize : unit
    default me.Initialize = me.SetToInitialized

    abstract member Update : uint32 -> unit

    member me.Abstract = me :> AbstractSystem


type SystemManager() =
    let mutable _systems = Array.empty<AbstractSystem>

    member _.Active = _systems |> Array.filter (fun s -> s.IsActive)
    member _.ActiveAndInitialized = _systems |> Array.filter (fun s -> s.IsActive && s.IsInitialized)
    member _.Get description = _systems |> Array.find (fun s -> s.Description = description)

    member me.AllSystemsIdle = 
        me.ActiveAndInitialized
        |> Array.forall (fun s -> s.IsIdle) 

    member me.Init (ss:AbstractSystem[]) =
        _systems <- ss
        me.Active |> Array.Parallel.iter (fun s -> s.Initialize)

    member me.UpdateSystems round =
        me.ActiveAndInitialized |> Array.Parallel.iter (fun s -> s.Update round)
        

