module agent_EntityManager
open agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component

type Save_Entities = 
    {
        Entities_Current : Map<EntityID,ComponentID[]>
        Entities_History : Map<EntityID,(RoundNumber*ComponentID[] option)[]>
    }

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

type agent_EntityManager(useHistory:bool, compMan:agent_Components) =
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
                        let toIDs (cts:Component[]) = cts |> Array.map GetComponentID
                        let add (cts:Component[]) =
                            let eid = GetComponentEntityID cts.[0]
                            _map <-
                                match _map.ContainsKey eid with
                                | true -> _map.Remove(eid).Add(eid,toIDs cts)
                                | false -> _map.Add(eid,toIDs cts)
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
                        let ctsToIDs (cts:Component[]) = cts |> Array.map GetComponentID
                        let add round (cts:Component[]) = 
                            let eid = GetComponentEntityID cts.[0]
                            _history <- 
                                match _history.ContainsKey eid with
                                | false -> _history.Add(eid,[|round,Some (ctsToIDs cts)|])
                                | true -> 
                                    let newArray = Array.append [|round,Some (ctsToIDs cts)|] (_history.Item eid)
                                    _history.Remove(eid).Add(eid,newArray)
                        let remove round eid = 
                            _history <-
                                match _history.ContainsKey eid with
                                | false -> _history.Add(eid,[|round,None|])
                                | true -> 
                                    let newArray = Array.append [|round,None|] (_history.Item eid)
                                    _history.Remove(eid).Add(eid,newArray)
                        match msg with
                        | History_Add (round,cts) -> add round cts
                        | History_AddMany (round,ctss) -> ctss |> Array.iter (add round)
                        | History_Init startMap -> _history <- startMap
                        | History_Remove (round,eid) -> remove round eid
                }
            )

    member _.Add (round:RoundNumber) cts = 
        Async.Parallel
        (
            agent_Current.Post (Add cts)
            if useHistory then agent_History.Post (History_Add (round,cts))
        )
    member _.AddMany (round:RoundNumber) ctss = 
        Async.Parallel
        (
            agent_Current.Post (AddMany ctss)
            if useHistory then agent_History.Post (History_AddMany (round,ctss))
        )
    member _.Get (round:RoundNumber option) eid = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel))
        | Some r,true -> getHistory r eid
        |> compMan.GetMany round
    member _.GetForSave =
        {
            Entities_Current = agent_Current.PostAndReply GetMap
            Entities_History = _history
        }
    member _.GetMany (round:RoundNumber option) eids = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> GetMany (eids,replyChannel))
        | Some r,true -> eids |> Array.map (getHistory r)
        |> Array.map (compMan.GetMany round)
    member _.Init (save:Save_Entities) =
        Async.Parallel
        (
            agent_Current.Post (Init save.Entities_Current)
            agent_History.Post (History_Init save.Entities_History)
        )
    member _.NewEntityID() = EntityID(idMan.GetNewID())
    member _.Remove (round:RoundNumber) eid = 
        Async.Parallel
        (
            agent_Current.Post (Remove eid)
            if useHistory then agent_History.Post (History_Remove (round,eid))
        )


