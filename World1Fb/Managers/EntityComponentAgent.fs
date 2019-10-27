module EntityComponentAgent
open AbstractComponent


type EntityIDAgentMsg = 
    | GetMax of AsyncReplyChannel<uint32>
    | GetNew of AsyncReplyChannel<uint32>
    | InitID of uint32


type NextAgentMsg = 
    | AddEntity of AbstractComponent[]
    | Exists of uint32 * AsyncReplyChannel<bool>
    | GetComponents of uint32 * AsyncReplyChannel<AbstractComponent[]>
    | GetMap of AsyncReplyChannel< Map<uint32,AbstractComponent[]> >
    | Init of Map<uint32,AbstractComponent[]>
    | RemoveEntity of uint32
    | ReplaceComponent of AbstractComponent

//type PreviousAgentMsg = 
//    //not sure I need this... | Prev_Exists of uint32 * AsyncReplyChannel<bool>
//    | Prev_GetComponents of uint32 * AsyncReplyChannel<AbstractComponent[]>
//    | Prev_GetMap of AsyncReplyChannel< Map<uint32,AbstractComponent[]> >
//    | Prev_Init of Map<uint32,AbstractComponent[]>


type EntityComponentAgent() = 

    let agentForID =
        let mutable _maxEntityID = 0u
        MailboxProcessor<EntityIDAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | GetMax replyChannel -> 
                            replyChannel.Reply(_maxEntityID)
                        | GetNew replyChannel -> 
                            _maxEntityID <- _maxEntityID + 1u
                            replyChannel.Reply(_maxEntityID)
                        | InitID startMax -> 
                            _maxEntityID <- startMax
                }
            )

    let agentForNext =
        let mutable _next = Map.empty<uint32,AbstractComponent[]>
        MailboxProcessor<NextAgentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        match msg with
                        | AddEntity cts ->
                            if not (_next.ContainsKey(cts.[0].EntityID)) then 
                                _next <- _next.Add(cts.[0].EntityID, cts)
                        | Exists (eid,replyChannel) ->
                            replyChannel.Reply(_next.ContainsKey eid)
                        | GetComponents (eid,replyChannel) ->
                            replyChannel.Reply(
                                match _next.ContainsKey eid with
                                | false -> [||]
                                | true -> _next.Item(eid)
                            )
                        | GetMap replyChannel ->
                            replyChannel.Reply(_next)
                        | Init newMap -> 
                            _next <- newMap
                        | RemoveEntity eid -> 
                            _next <- _next.Remove eid
                        | ReplaceComponent ct ->
                            if (_next.ContainsKey ct.EntityID) then
                                _next <-
                                    let a = 
                                        _next.Item(ct.EntityID)
                                        |> Array.filter (fun ac -> ac.ComponentType <> ct.ComponentType)
                                        |> Array.append [|ct|]
                                    _next.Remove(ct.EntityID).Add(ct.EntityID,a)
                }
            )

    //let agentForPrevious =
    //    let mutable _previous = Map.empty<uint32,AbstractComponent[]>
    //    MailboxProcessor<PreviousAgentMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    //| Prev_Exists (eid,replyChannel) ->
    //                    //    replyChannel.Reply(_previous.ContainsKey eid)
    //                    | Prev_GetComponents (eid,replyChannel) ->
    //                        replyChannel.Reply(
    //                            match _previous.ContainsKey eid with
    //                            | false -> [||]
    //                            | true -> _previous.Item(eid)
    //                        )
    //                    | Prev_GetMap replyChannel ->
    //                        replyChannel.Reply(_previous)
    //                    | Prev_Init newMap -> 
    //                        _previous <- newMap
    //            }
    //        )

    member this.CreateEntity (cts:AbstractComponent[]) = 
        agentForNext.Post (AddEntity cts)

    member this.Exists (eid:uint32) = 
        agentForNext.PostAndReply (fun replyChannel -> Exists(eid,replyChannel))

    member this.GetComponents (eid:uint32) = 
        agentForNext.PostAndReply (fun replyChannel -> GetComponents(eid,replyChannel))

    member this.GetMap() = 
        agentForNext.PostAndReply GetMap

    member this.GetMaxID = 
        agentForID.PostAndReply GetMax
    
    member this.GetNewID = 
        agentForID.PostAndReply GetNew
    
    member this.RemoveEntity (eid:uint32) = 
        agentForNext.Post (RemoveEntity eid)

    member this.ReplaceComponent (ct:AbstractComponent) =
        agentForNext.Post (ReplaceComponent ct)

    member this.Init (startMax:uint32) (newMap:Map<uint32,AbstractComponent[]>) = 
        agentForID.Post (InitID startMax)
        agentForNext.Post (Init newMap)


//member this.List () =
//    _entDict |> Map.iter (fun k v -> printfn "%i | %i" k v.List.Length)
       


