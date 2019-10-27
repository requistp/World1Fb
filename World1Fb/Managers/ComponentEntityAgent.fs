module ComponentEntityAgent
open AbstractComponent
open EntityAgent


type ComponentEntityAgentMsg = 
    | Add of AbstractComponent
    | Get of ComponentTypes * AsyncReplyChannel<uint32[]>
    | GetMap of AsyncReplyChannel<Map<ComponentTypes,uint32[]> >
    | Init of Map<ComponentTypes,uint32[]>    
    | Remove of AbstractComponent


type ComponentEntityAgent() = 

    let agent =
        let mutable _compDict = 
            ComponentTypes.AsArray
            |> Array.fold (fun (m:Map<ComponentTypes,EntityAgent>) ct -> m.Add(ct,new EntityAgent())) Map.empty

        MailboxProcessor<ComponentEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add ct ->
                            _compDict.Item(ct.ComponentType).Add ct.EntityID
                        | Get (componentType,replyChannel) -> 
                            replyChannel.Reply(_compDict.Item(componentType).Get)
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_compDict |> Map.map (fun k v -> v.Get))
                        | Init m -> 
                            m |> Map.iter (fun k v -> _compDict.Item(k).Init v)
                        | Remove ct ->
                            _compDict.Item(ct.ComponentType).Remove ct.EntityID
                }
            )

    member this.Add (ct:AbstractComponent) = 
        agent.Post (Add ct)

    member this.Add (cts:AbstractComponent[]) =
        cts |> Array.Parallel.iter (fun ct -> this.Add ct)
    
    member this.Get (componentType:ComponentTypes) = 
        agent.PostAndReply (fun replyChannel -> Get (componentType,replyChannel))

    member this.GetMap() =
        agent.PostAndReply GetMap
        
    member this.Init (newMap:Map<ComponentTypes,uint32[]>) =
        agent.Post (Init newMap)
    
    member this.Print =
        this.GetMap()
        |> Map.iter (fun k v -> printfn "%s | %i" (k.ToString()) v.Length)

    member this.Remove (ct:AbstractComponent) =
        agent.Post (Remove ct)

    member this.Remove (cts:AbstractComponent[]) =
        cts |> Array.Parallel.iter (fun ct -> this.Remove ct)

 
