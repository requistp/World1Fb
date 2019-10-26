module EntityDictionary


type EntityDictionaryMsg = 
    | Add of uint32
    | Remove of uint32
    | List of AsyncReplyChannel<uint32[]>


type EntityDictionary() =
    let listAgent =
        let mutable _array = Array.empty<uint32>

        let add eid =
            match _array |> Array.exists (fun e -> e = eid) with 
            | true -> ()
            | false -> _array <- [|eid|] |> Array.append _array

        let remove eid =
            match _array |> Array.exists (fun e -> e = eid) with 
            | false -> ()
            | true -> _array <- _array |> Array.filter (fun e -> e <> eid)

        MailboxProcessor<EntityDictionaryMsg>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! msg = inbox.Receive()
                            match msg with
                            | Add eid -> add eid
                            | Remove eid -> remove eid
                            | List replyChannel -> replyChannel.Reply(_array)
                    }
            )
    
    member this.Add eid = listAgent.Post (Add eid)

    member this.Remove eid = listAgent.Post (Remove eid)

    member this.List = listAgent.PostAndReply List 
