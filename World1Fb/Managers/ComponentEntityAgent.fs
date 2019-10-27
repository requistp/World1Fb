module ComponentEntityAgent
open AbstractComponent
open CommonGenericFunctions


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
            |> Array.fold (fun (m:Map<ComponentTypes,uint32[]>) ct -> m.Add(ct,Array.empty)) Map.empty

        MailboxProcessor<ComponentEntityAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add ct ->
                            match _compDict.Item(ct.ComponentType)|>Array.contains ct.EntityID with
                            | true -> ()
                            | false -> _compDict <- Map_AppendValueToArray _compDict ct.ComponentType ct.EntityID
                        | Get (componentType,replyChannel) -> 
                            replyChannel.Reply(_compDict.Item(componentType))
                        | GetMap replyChannel -> 
                            replyChannel.Reply(_compDict)
                        | Init newMap -> 
                            _compDict <- newMap
                        | Remove ct ->
                            _compDict <-
                                let a = _compDict.Item(ct.ComponentType) |> Array.filter (fun eid -> eid <> ct.EntityID)
                                _compDict.Remove(ct.ComponentType).Add(ct.ComponentType,a)
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

 
 -