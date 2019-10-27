module ComponentAgent
open AbstractComponent

// I left out the Remove until it is in use

type ComponentAgentMsg = 
    | Add of AbstractComponent
    | AddAll of AbstractComponent[]
    | Get of AsyncReplyChannel<AbstractComponent[]>
    //| Remove of AbstractComponent
    //| RemoveAll of AbstractComponent[]
    | Replace of AbstractComponent


type ComponentAgent(cts:AbstractComponent[]) =

    let agent =
        let mutable _array = cts
                
        //let remove (ct:AbstractComponent) =
        //    _array <- _array |> Array.filter (fun ac -> ac.ComponentType <> ct.ComponentType)
            
        MailboxProcessor<ComponentAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | Add ct -> 
                            _array <- Array.append _array [|ct|]
                        | AddAll cts -> 
                            _array <- Array.append _array cts
                        | Get replyChannel -> 
                            replyChannel.Reply(_array)
                        //| Remove ct -> 
                        //    remove ct
                        //| RemoveAll cts -> 
                        //    //cts |> Array.iter (fun ct -> remove ct)
                        //    _array <- 
                        //        _array |> Array.filter (fun ct -> cts|>Array.contains ct) //   ac.ComponentType <> ct.ComponentType)
                        | Replace ct ->
                            _array <- 
                                _array 
                                |> Array.filter (fun ac -> ac.ComponentType <> ct.ComponentType)
                                |> Array.append [|ct|]
                }
            )
    
    member this.Add ct = agent.Post (Add ct)

    member this.Add cts = agent.Post (AddAll cts)

    //member this.Remove ct = agent.Post (Remove ct)

    //member this.Remove cts = agent.Post (RemoveAll cts)

    member this.Replace ct = agent.Post (Replace ct)

    member this.Get = agent.PostAndReply Get 

    