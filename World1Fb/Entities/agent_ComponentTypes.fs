module agent_ComponentTypes
open agent_Components
open CommonGenericFunctions
open Component
open ComponentEnums


type private agent_CurrentMsg = 
    | Add of Component
    | AddMany of Component[]
    | Get of ComponentType * AsyncReplyChannel<ComponentID[]>
    | GetMap of AsyncReplyChannel<Map<ComponentType,ComponentID[]> >
    | Init of Map<ComponentType,ComponentID[]>
    | Remove of Component
    | RemoveMany of Component[]
        
type private agent_HistoryMsg = 
    | History_Add of RoundNumber * Component
    | History_AddMany of RoundNumber * Component[]
    | History_Init of Map<ComponentType,(RoundNumber*ComponentID[] option)[]>
    | History_Remove of RoundNumber * Component
    | History_RemoveMany of RoundNumber * Component[]
        
type agent_ComponentTypes(useHistory:bool, compMan:agent_Components) = 
    let mutable _history = Map.empty<ComponentType,(RoundNumber*ComponentID[] option)[]>

    let getHistory round ctid = 
        match (_history.ContainsKey ctid) with
        | false -> [||]
        | true -> 
            match searchArrayDataForRound round (_history.Item ctid) with
            | None -> [||]
            | Some a -> a

    let agent_Current =
        let mutable _map = Map.empty<ComponentType,ComponentID[]>
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add (comp:Component) = 
                            _map <- 
                                match _map.ContainsKey(GetComponentType comp) with
                                | false -> _map.Add(GetComponentType comp,[|GetComponentID comp|])
                                | true -> 
                                    let others = _map.Item(GetComponentType comp) |> Array.filter (fun c -> c <> GetComponentID comp) // In case component was already here
                                    _map.Remove(GetComponentType comp).Add(GetComponentType comp,Array.append others [|GetComponentID comp|]) 
                        let remove (comp:Component) =
                            if (_map.ContainsKey(GetComponentType comp)) then
                                let others = _map.Item(GetComponentType comp) |> Array.filter (fun c -> c <> GetComponentID comp)
                                _map <- _map.Remove(GetComponentType comp).Add(GetComponentType comp,others) 
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
                                match _history.ContainsKey(GetComponentType c) with
                                | false -> _history.Add(GetComponentType c,[|round,Some [|GetComponentID c|]|])
                                | true -> 
                                    let h,t = _history.Item(GetComponentType c) |> Array.splitAt 1
                                    let newArray = 
                                        match ((fst h.[0]) = round) with
                                        | false -> Array.append [|round,Some [|GetComponentID c|]|] t
                                        | true -> Array.append [|round,Some [|GetComponentID c|]|] (_history.Item(GetComponentType c))
                                    _history.Remove(GetComponentType c).Add(GetComponentType c,newArray)
                        let remove round (c:Component) =
                            if (_history.ContainsKey(GetComponentType c)) then
                                match snd (_history.Item(GetComponentType c)).[0] with
                                | None -> ()
                                | Some a ->
                                    if a |> Array.contains(GetComponentID c) then
                                        _history <-
                                            match a |> Array.filter (fun id -> id <> GetComponentID c) with
                                            | [||] -> _history.Add(GetComponentType c,[|round,None|])
                                            | n -> _history.Add(GetComponentType c,[|round,Some n|])
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
            if useHistory then agent_History.Post (History_Add (round,comp))
        )
    member _.AddMany (round:RoundNumber) cts = 
        Async.Parallel
        (
            agent_Current.Post (AddMany cts)
            if useHistory then agent_History.Post (History_AddMany (round,cts))
        )
    member _.Get round ctid = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
        | Some r,true -> getHistory r ctid
        |> compMan.GetMany round
    member _.GetForSave =
        agent_Current.PostAndReply GetMap
        ,
        _history
    member _.GetMap (round:RoundNumber option) = 
        match round,useHistory with
        | None,_ | Some _,false -> 
            agent_Current.PostAndReply GetMap
            |> Map.map (fun _ cids -> cids |> Array.choose (compMan.Get round))
        | Some _,true -> 
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
            if useHistory then agent_History.Post (History_Init historyMap)
        )
    member _.Remove (round:RoundNumber) (comp:Component) = 
        Async.Parallel
        (
            agent_Current.Post (Remove comp)
            if useHistory then agent_History.Post (History_Remove (round,comp))
        )
    member _.RemoveMany (round:RoundNumber) (cts:Component[]) = 
        Async.Parallel
        (
            agent_Current.Post (RemoveMany cts)
            if useHistory then agent_History.Post (History_RemoveMany (round,cts))
        )



    //member _.GetIDs round ctid = 
    //    match round with
    //    | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
    //    | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) // FIX    //member _.Add round ctid cid = agent_History.Post (AddHistory (round,ctid,cid))
    //member _.Get round ctid = agent_History.PostAndReply (fun replyChannel -> GetHistory (ctid,replyChannel)) |> searchArrayDataForRound round
    //member _.Remove round (comp:Component) = agent_History.Post (RemoveHistory (round,comp.ComponentTypeID,comp.ID))

