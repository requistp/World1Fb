module EntityArray


type EntityArrayMsg = 
    | Add of uint32
    | Remove of uint32
    | List of AsyncReplyChannel<uint32[]>


type EntityArray() =
    let listAgent =
        let mutable _entities = Array.empty<uint32>
        let add eid =
            match _entities |> Array.exists (fun e -> e = eid) with 
            | true -> ()
            | false -> _entities <- [|eid|] |> Array.append _entities
        let remove eid =
            match _entities |> Array.exists (fun e -> e = eid) with 
            | false -> ()
            | true -> _entities <- _entities |> Array.filter (fun e -> e <> eid)
        MailboxProcessor<EntityArrayMsg>.Start(
            fun inbox ->
                async 
                    { 
                        while true do
                            let! msg = inbox.Receive()
                            match msg with
                            | Add eid -> add eid
                            | Remove eid -> remove eid
                            | List replyChannel -> replyChannel.Reply(_entities)
                    }
            )
    
    member this.Add eid = listAgent.Post (Add eid)
    member this.Remove eid = listAgent.Post (Remove eid)
    member this.List = listAgent.PostAndReply List 
