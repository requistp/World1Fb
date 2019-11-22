module agent_ComponentTypes
open agent_Components
open CommonGenericFunctions
open Component


//type private agent_HistoryMsg = 
//    | AddHistory of RoundNumber * ComponentTypeID * ComponentID
//    | GetHistory of ComponentTypeID * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
//    //| GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
//    | RemoveHistory of RoundNumber * ComponentTypeID * ComponentID

    
type private agent_CurrentMsg = 
    | Add of Component
    | AddMany of Component[]
    | Get of ComponentTypeID * AsyncReplyChannel<ComponentID[]>
    //| GetComponentMap of AsyncReplyChannel<Map<byte,uint32[]> >
    | Remove of Component
    | RemoveMany of Component[]
    
    
type agent_ComponentTypes(compMan:agent_Components) = 

    //let agent_History =
    //    let mutable _map = Map.empty<ComponentTypeID,(RoundNumber*ComponentID[] option)[]>
    //    MailboxProcessor<agent_HistoryMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    | AddHistory (round,ctid,cid) ->
    //                        _map <-
    //                            match _map.ContainsKey(ctid) with
    //                            | false -> _map.Add(ctid,[|(round,Some [|cid|])|])
    //                            | true -> 
    //                                let oldArray = _map.Item(ctid)
    //                                let addNew =
    //                                    match (snd oldArray.[0]) with
    //                                    | None -> [|(round,Some [|cid|])|]
    //                                    | Some eids -> [|(round, Some (Array.append eids [|cid|]))|]
    //                                _map.Remove(ctid).Add(ctid,Array.append addNew oldArray) 
    //                    | GetHistory (ctid,replyChannel) -> 
    //                        replyChannel.Reply(
    //                            match _map.ContainsKey(ctid) with
    //                            | false -> Array.empty
    //                            | true -> _map.Item(ctid))
    //                    //| GetComponentMap replyChannel -> 
    //                    //    replyChannel.Reply(_map)
    //                    | RemoveHistory (round,ctid,cid) ->
    //                        match _map.ContainsKey(ctid) with
    //                        | false -> ()
    //                        | true -> 
    //                            let history = _map.Item(ctid)
    //                            match (snd history.[0]) with 
    //                            | None -> ()
    //                            | Some a ->
    //                                match Array.contains cid a with 
    //                                | false -> ()
    //                                | true -> 
    //                                    let newArray = 
    //                                        match a |> Array.filter (fun c -> c <> cid) with 
    //                                        | [||] -> None
    //                                        | filtered -> Some filtered
    //                                    _map <- _map.Remove(ctid).Add(ctid,Array.append [|(round,newArray)|] history)
    //            }
    //        )

    let agent_Current =
        let mutable _map = Map.empty<ComponentTypeID,ComponentID[]>
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add (comp:Component) = 
                            _map <- 
                                match _map.ContainsKey comp.ComponentTypeID with
                                | false -> _map.Add(comp.ComponentTypeID,[|comp.ID|])
                                | true -> 
                                    let others = _map.Item(comp.ComponentTypeID) |> Array.filter (fun c -> c <> comp.ID) // In case component was already here
                                    _map.Remove(comp.ComponentTypeID).Add(comp.ComponentTypeID,Array.append others [|comp.ID|]) 
                        let remove (comp:Component) =
                            if (_map.ContainsKey comp.ComponentTypeID) then
                                let others = _map.Item(comp.ComponentTypeID) |> Array.filter (fun c -> c <> comp.ID)
                                _map <- _map.Remove(comp.ComponentTypeID).Add(comp.ComponentTypeID,others) 
                        match msg with
                        | Add comp -> add comp
                        | AddMany cts -> cts |> Array.iter add
                        | Get (ctid,replyChannel) -> 
                            replyChannel.Reply(
                                match _map.ContainsKey ctid with
                                | false -> Array.empty
                                | true -> _map.Item(ctid))
                        //| GetComponentMap replyChannel -> 
                        //    replyChannel.Reply(_map)
                        | Remove comp -> remove comp
                        | RemoveMany cts -> cts |> Array.iter remove
                }
            )
    member _.Add (round:RoundNumber) comp = 
        agent_Current.Post (Add comp)
        // FIX history
    member _.AddMany (round:RoundNumber) cts = 
        agent_Current.Post (AddMany cts)
        // FIX history
    member _.Get round ctid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) // FIX
        |> compMan.GetMany round
    member _.GetIDs round ctid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) // FIX
    member _.Remove (round:RoundNumber) (comp:Component) = 
        agent_Current.Post (Remove comp)
        // FIX history
    member _.RemoveMany (round:RoundNumber) (cts:Component[]) = 
        agent_Current.Post (RemoveMany cts)
        // FIX history

    //member _.Add round ctid cid = agent_History.Post (AddHistory (round,ctid,cid))
    //member _.Get round ctid = agent_History.PostAndReply (fun replyChannel -> GetHistory (ctid,replyChannel)) |> searchArrayDataForRound round
    //member _.Remove round (comp:Component) = agent_History.Post (RemoveHistory (round,comp.ComponentTypeID,comp.ID))

