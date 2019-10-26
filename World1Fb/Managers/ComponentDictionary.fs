module ComponentDictionary
open AbstractComponent


type ComponentDictionaryMsg = 
    | Add of AbstractComponent
    | AddAll of AbstractComponent[]
    | Remove of AbstractComponent
    | RemoveAll of AbstractComponent[]
    | Replace of AbstractComponent
    | List of AsyncReplyChannel<AbstractComponent[]>


type ComponentDictionary(cts:AbstractComponent[]) =
    let listAgent =
        let mutable _array = cts

        let add (ct:AbstractComponent) =
            _array <- [|ct|] |> Array.append _array

        let addAll (cts:AbstractComponent[]) =
            cts |> Array.iter (fun ct -> add ct)
        
        let remove (ct:AbstractComponent) =
            match _array |> Array.exists (fun ac -> ac.ComponentType = ct.ComponentType) with 
            | false -> ()
            | true -> _array <- _array |> Array.filter (fun ac -> ac.ComponentType <> ct.ComponentType)

        let removeAll (cts:AbstractComponent[]) =
            cts |> Array.iter (fun ct -> remove ct)

        let replace (ct:AbstractComponent) =
            if (_array |> Array.exists (fun ac -> ac.ComponentType = ct.ComponentType)) then remove ct
            _array <- [|ct|] |> Array.append _array

        MailboxProcessor<ComponentDictionaryMsg>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! msg = inbox.Receive()
                            match msg with
                            | Add ct -> add ct
                            | AddAll cts -> addAll cts
                            | Remove ct -> remove ct
                            | RemoveAll cts -> removeAll cts
                            | Replace ct -> replace ct
                            | List replyChannel -> replyChannel.Reply(_array)
                    }
            )
    
    member this.Add ct = listAgent.Post (Add ct)

    member this.Add cts = listAgent.Post (AddAll cts)

    member this.Remove ct = listAgent.Post (Remove ct)

    member this.Remove cts = listAgent.Post (RemoveAll cts)

    member this.Replace ct = listAgent.Post (Replace ct)

    member this.List = listAgent.PostAndReply List 

    