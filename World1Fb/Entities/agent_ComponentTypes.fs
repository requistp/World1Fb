module agent_ComponentTypes
open agent_Components
open CommonGenericFunctions
open Component


type private agent_CurrentMsg = 
    | Add of Component
    | AddMany of Component[]
    | Get of ComponentTypeID * AsyncReplyChannel<ComponentID[]>
    | GetMap of AsyncReplyChannel<Map<ComponentTypeID,ComponentID[]> >
    | Init of Map<ComponentTypeID,ComponentID[]>
    | Remove of Component
    | RemoveMany of Component[]
        
type private agent_HistoryMsg = 
    | History_Add of RoundNumber * Component
    | History_AddMany of RoundNumber * Component[]
    | History_Init of Map<ComponentTypeID,(RoundNumber*ComponentID[] option)[]>
    | History_Remove of RoundNumber * Component
    | History_RemoveMany of RoundNumber * Component[]
        
type agent_ComponentTypes(compMan:agent_Components) = 
    let mutable _history = Map.empty<ComponentTypeID,(RoundNumber*ComponentID[] option)[]>

    let getHistory round ctid = 
        match (_history.ContainsKey ctid) with
        | false -> [||]
        | true -> 
            match searchArrayDataForRound round (_history.Item ctid) with
            | None -> [||]
            | Some a -> a

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
                                | true -> _map.Item ctid)
                        | GetMap replyChannel -> replyChannel.Reply(_map)
                        | Init startMap -> _map <- startMap
                        | Remove comp -> remove comp
                        | RemoveMany cts -> cts |> Array.iter remove
                }
            )
    
    let agent_History =
        MailboxProcessor<agent_HistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add round (c:Component) = 
                            _history <- 
                                match _history.ContainsKey c.ComponentTypeID with
                                | false -> _history.Add(c.ComponentTypeID,[|round,Some [|c.ID|]|])
                                | true -> 
                                    let h,t = _history.Item(c.ComponentTypeID) |> Array.splitAt 1
                                    let newArray = 
                                        match ((fst h.[0]) = round) with
                                        | false -> Array.append [|round,Some [|c.ID|]|] t
                                        | true -> Array.append [|round,Some [|c.ID|]|] (_history.Item c.ComponentTypeID)
                                    _history.Remove(c.ComponentTypeID).Add(c.ComponentTypeID,newArray)
                        let remove round (c:Component) =
                            if (_history.ContainsKey c.ComponentTypeID) then
                                match snd (_history.Item c.ComponentTypeID).[0] with
                                | None -> ()
                                | Some a ->
                                    if a |> Array.contains c.ID then
                                        _history <-
                                            match a |> Array.filter (fun id -> id <> c.ID) with
                                            | [||] -> _history.Add(c.ComponentTypeID,[|round,None|])
                                            | n -> _history.Add(c.ComponentTypeID,[|round,Some n|])
                        match msg with
                        | History_Add (round,c) -> add round c
                        | History_AddMany (round,cs) -> cs |> Array.iter (add round)
                        | History_Init startMap -> _history <- startMap
                        | History_Remove (round,c) -> remove round c
                        | History_RemoveMany (round,cs) -> cs |> Array.iter (remove round)
                }
            )
    
    member _.Add (round:RoundNumber) comp = 
        Async.Parallel
        (
            agent_Current.Post (Add comp)
            agent_History.Post (History_Add (round,comp))
        )
    member _.AddMany (round:RoundNumber) cts = 
        Async.Parallel
        (
            agent_Current.Post (AddMany cts)
            agent_History.Post (History_AddMany (round,cts))
        )
    member _.Get round ctid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
        | Some r -> getHistory r ctid
        |> compMan.GetMany round
    member _.GetForSave =
        agent_Current.PostAndReply GetMap
        ,
        _history
    member _.GetMap (round:RoundNumber option) = 
        match round with
        | None -> 
            agent_Current.PostAndReply GetMap
            |> Map.map (fun _ cids -> cids |> Array.choose (compMan.Get round))
        | Some _ -> 
            _history
            |> Map.map (fun _ a -> 
                match (snd a.[0]) with
                | None -> [||]
                | Some cids ->
                    cids |> Array.choose (compMan.Get round)
                )
    member _.Init currentMap historyMap =
        Async.Parallel
        (
            agent_Current.Post (Init currentMap)
            agent_History.Post (History_Init historyMap)
        )
    member _.Remove (round:RoundNumber) (comp:Component) = 
        Async.Parallel
        (
            agent_Current.Post (Remove comp)
            agent_History.Post (History_Remove (round,comp))
        )
    member _.RemoveMany (round:RoundNumber) (cts:Component[]) = 
        Async.Parallel
        (
            agent_Current.Post (RemoveMany cts)
            agent_History.Post (History_RemoveMany (round,cts))
        )



    //member _.GetIDs round ctid = 
    //    match round with
    //    | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
    //    | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) // FIX    //member _.Add round ctid cid = agent_History.Post (AddHistory (round,ctid,cid))
    //member _.Get round ctid = agent_History.PostAndReply (fun replyChannel -> GetHistory (ctid,replyChannel)) |> searchArrayDataForRound round
    //member _.Remove round (comp:Component) = agent_History.Post (RemoveHistory (round,comp.ComponentTypeID,comp.ID))

