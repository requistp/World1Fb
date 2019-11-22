module agent_EntityManager
open agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component

type private agent_CurrentMsg = 
    | Add of Component[]
    | AddMany of Component[][]
    | Get of EntityID * AsyncReplyChannel<ComponentID[]>
    | GetMany of EntityID[] * AsyncReplyChannel<ComponentID[][]>
    | GetMap of AsyncReplyChannel<Map<EntityID,ComponentID[]> >
    | Init of Map<EntityID,ComponentID[]>
    | Remove of EntityID

type private agent_HistoryMsg = 
    | History_Add of RoundNumber * Component[]
    | History_AddMany of RoundNumber * Component[][]
    | History_Init of Map<EntityID,(RoundNumber*ComponentID[] option)[]>
    | History_Remove of RoundNumber * EntityID

type agent_EntityManager(compMan:agent_Components) =
    let mutable _history = Map.empty<EntityID,(RoundNumber*ComponentID[] option)[]>
    
    let getHistory round eid = 
        match (_history.ContainsKey eid) with
        | false -> [||]
        | true -> 
            match searchArrayDataForRound round (_history.Item eid) with
            | None -> [||]
            | Some a -> a

    let idMan = new agent_IDManager()
    
    let agent_Current =
        let mutable _map = Map.empty<EntityID,ComponentID[]>
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let toIDs (cts:Component[]) = cts |> Array.map (fun c -> c.ID)
                        let add (cts:Component[]) =
                            _map <-
                                match _map.ContainsKey cts.[0].EntityID with
                                | true -> _map.Remove(cts.[0].EntityID).Add(cts.[0].EntityID,toIDs cts)
                                | false -> _map.Add(cts.[0].EntityID,toIDs cts)
                        let get eid =
                            match _map.ContainsKey eid with
                            | false -> Array.empty
                            | true -> _map.Item eid
                        match msg with
                        | Add cts -> add cts
                        | AddMany ctss -> ctss |> Array.iter add
                        | Get (eid,replyChannel) -> replyChannel.Reply(get eid)
                        | GetMany (eids,replyChannel) -> replyChannel.Reply(eids |> Array.map get)
                        | GetMap replyChannel -> replyChannel.Reply(_map)
                        | Init startMap -> _map <- startMap
                        | Remove eid ->
                            if (_map.ContainsKey eid) then
                                _map <- _map.Remove(eid)
                }
            )

    let agent_History =
        MailboxProcessor<agent_HistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let ctsToIDs (cts:Component[]) = cts |> Array.map (fun c -> c.ID)
                        let add round (cts:Component[]) = 
                            let eid = cts.[0].EntityID
                            _history <- 
                                match _history.ContainsKey eid with
                                | false -> _history.Add(eid,[|round,Some (ctsToIDs cts)|])
                                | true -> 
                                    let h,t = _history.Item(eid) |> Array.splitAt 1
                                    let newArray = 
                                        match (fst h.[0] = round) with
                                        | false -> Array.append [|round,Some (ctsToIDs cts)|] t
                                        | true -> Array.append [|round,Some (ctsToIDs cts)|] (_history.Item cts.[0].EntityID)
                                    _history.Remove(eid).Add(eid,newArray)
                        match msg with
                        | History_Add (round,cts) -> add round cts
                        | History_AddMany (round,ctss) -> ctss |> Array.iter (add round)
                        | History_Init startMap -> _history <- startMap
                        | History_Remove (round,eid) ->
                            if (_history.ContainsKey eid) then
                                match snd (_history.Item eid).[0] with
                                | None -> ()
                                | Some _ -> _history <- _history.Add(eid,[|round,None|])
                }
            )

    member _.Add (round:RoundNumber) cts = 
        Async.Parallel
        (
            agent_Current.Post (Add cts)
            agent_History.Post (History_Add (round,cts))
        )
    member _.AddMany (round:RoundNumber) ctss = 
        Async.Parallel
        (
            agent_Current.Post (AddMany ctss)
            agent_History.Post (History_AddMany (round,ctss))
        )
    member _.Get (round:RoundNumber option) eid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel))
        | Some r -> getHistory r eid
        |> compMan.GetMany round
    member _.GetForSave =
        agent_Current.PostAndReply GetMap
        ,
        _history
    member _.GetMany (round:RoundNumber option) eids = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> GetMany (eids,replyChannel))
        | Some r -> eids |> Array.map (getHistory r)
        |> Array.map (compMan.GetMany round)
    member _.Init currentMap historyMap =
        Async.Parallel
        (
            agent_Current.Post (Init currentMap)
            agent_History.Post (History_Init historyMap)
        )
    member _.NewEntityID() = EntityID(idMan.GetNewID())
    member _.Remove (round:RoundNumber) eid = 
        Async.Parallel
        (
            agent_Current.Post (Remove eid)
            agent_History.Post (History_Remove (round,eid))
        )


