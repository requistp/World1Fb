module SystemManager
open CommonGenericFunctions
open EventTypes

type private agent_SystemWorkTrackerMsg = 
    | Decrement
    | Increment
    | IsIdle of AsyncReplyChannel<bool>
    
[<AbstractClass>]
type AbstractSystem(description:string, isActive:bool) =
    let mutable _isInitialized = false

    let agentWork = 
        let mutable _tasks = 0u
        MailboxProcessor<agent_SystemWorkTrackerMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Decrement -> 
                            _tasks <- _tasks - 1u
                        | Increment -> 
                            _tasks <- _tasks + 1u
                        | IsIdle replyChannel ->
                            replyChannel.Reply(inbox.CurrentQueueLength = 0 && _tasks = 0u)
                }
            )

    member _.Description = description
    member _.IsActive = isActive
    member _.IsIdle = agentWork.PostAndReply IsIdle
    member _.IsInitialized = _isInitialized
    member _.SetToInitialized = _isInitialized <- true
    member _.TrackTask (task:RoundNumber->GameEventTypes->Result<string option,string>) (round:RoundNumber) (ge:GameEventTypes) = 
        agentWork.Post Increment
        let result = task round ge
        agentWork.Post Decrement
        result

    abstract member Initialize : unit
    default me.Initialize = me.SetToInitialized

    abstract member Update : RoundNumber -> unit

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
        

