module agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component

type Save_Components = 
    {
        Components_Current : Map<ComponentID,Component>
        Components_History : Map<ComponentID,(RoundNumber*Component option)[]>
    }

type private agent_CurrentMsg = 
    | Add of Component
    | AddMany of Component[]
    | Get of ComponentID * AsyncReplyChannel<Component option>
    | GetMany of ComponentID[] * AsyncReplyChannel<Component[]>
    | GetMap of AsyncReplyChannel<Map<ComponentID,Component> >
    | Init of Map<ComponentID,Component>
    | Remove of Component
    | RemoveMany of Component[]
    | Update of Component

type private agent_HistoryMsg =
    | History_Add of RoundNumber * Component
    | History_AddMany of RoundNumber * Component[]
    | History_Init of Map<ComponentID,(RoundNumber*Component option)[]>
    | History_Remove of RoundNumber * Component
    | History_RemoveMany of RoundNumber * Component[]
    
type agent_Components(useHistory:bool) =
    let mutable _history = Map.empty<ComponentID,(RoundNumber*Component option)[]>

    let getHistory round cid = 
        match (_history.ContainsKey cid) with
        | false -> None
        | true -> searchArrayDataForRound round (_history.Item cid)

    let idMan = new agent_IDManager()

    let agent_Current =
        let mutable _map = Map.empty<ComponentID,Component>
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add (comp:Component) = 
                            _map <-
                                match _map.ContainsKey(GetComponentID comp) with
                                | false -> _map.Add(GetComponentID comp,comp)
                                | true -> _map.Remove((GetComponentID comp)).Add((GetComponentID comp),comp)
                        let get cid =
                            match _map.ContainsKey cid with
                            | false -> None
                            | true -> Some (_map.Item cid)
                        let remove (comp:Component) =
                            if (_map.ContainsKey(GetComponentID comp)) then
                                _map <- _map.Remove(GetComponentID comp)
                        match msg with
                        | Add comp -> add comp
                        | AddMany cts -> cts |> Array.iter add
                        | Get (cid,replyChannel) -> replyChannel.Reply(get cid)
                        | GetMany (cids,replyChannel) -> replyChannel.Reply(cids |> Array.choose get)
                        | GetMap replyChannel -> replyChannel.Reply(_map)
                        | Init startMap ->
                            _map <- startMap
                        | Remove c -> remove c
                        | RemoveMany cts -> cts |> Array.iter remove
                        | Update comp ->
                            _map <-
                                match _map.ContainsKey(GetComponentID comp) with
                                | false -> _map.Add(GetComponentID comp,comp)
                                | true -> _map.Remove(GetComponentID comp).Add(GetComponentID comp,comp)
                }
            )

    let agent_History =
        MailboxProcessor<agent_HistoryMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add round (c:Component) =
                            let cid = GetComponentID c
                            _history <- 
                                match (_history.ContainsKey cid) with
                                | false -> _history.Add(cid,[|round,Some c|])
                                | true -> 
                                    let newArray = Array.append [|round,Some c|] (_history.Item cid)
                                    _history.Remove(cid).Add(cid,newArray)
                        let remove round (c:Component) =
                            let cid = GetComponentID c
                            _history <- 
                                match (_history.ContainsKey cid) with
                                | false -> _history.Add(cid,[|round,None|]) // I track this because it might help detect if something was not added (which should have happened before this)
                                | true ->
                                    let newArray = Array.append [|round,None|] (_history.Item cid)
                                    _history.Add(cid,newArray)
                        match msg with
                        | History_Add (round,comp) -> add round comp
                        | History_AddMany (round,comps) -> comps |> Array.iter (add round)
                        | History_Init historyMap -> _history <- historyMap
                        | History_Remove (round,c) -> remove round c
                        | History_RemoveMany (round,cts) -> cts |> Array.iter (remove round)
                }
            )

    member _.Add (round:RoundNumber) comp = 
        Async.Parallel
        (
            agent_Current.Post (Add comp)
            if useHistory then agent_History.Post (History_Add (round,comp))
        )
    member _.AddMany (round:RoundNumber) comps = 
        Async.Parallel
        (
            agent_Current.Post (AddMany comps)
            if useHistory then agent_History.Post (History_AddMany (round,comps))
        )
    member _.Get (round:RoundNumber option) cid = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> Get (cid,replyChannel))
        | Some r,true -> getHistory r cid
    member _.GetForSave =
        {
            Components_Current = agent_Current.PostAndReply GetMap
            Components_History = _history
        }
    member _.GetMany (round:RoundNumber option) cids = 
        match round,useHistory with
        | None,_ | Some _,false -> agent_Current.PostAndReply (fun replyChannel -> GetMany (cids,replyChannel))
        | Some r,true -> cids |> Array.choose (getHistory r)
    member _.NewComponentID() = ComponentID(idMan.GetNewID())
    member _.Init (save:Save_Components) =
        Async.Parallel
        (
            agent_Current.Post (Init save.Components_Current)
            idMan.Init (MapKeys save.Components_Current |> Seq.map (fun k -> k.ToUint32) |> Seq.max)
            agent_History.Post (History_Init save.Components_History)
        )
    member _.Remove (round:RoundNumber) c = 
        Async.Parallel
        (
            agent_Current.Post (Remove c)
            if useHistory then agent_History.Post (History_Remove (round,c))
        )
    member _.RemoveMany (round:RoundNumber) cts = 
        Async.Parallel
        (
            agent_Current.Post (RemoveMany cts)
            if useHistory then agent_History.Post (History_RemoveMany (round,cts))
        )
    member _.Update (round:RoundNumber) comp = 
        Async.Parallel
        (
            agent_Current.Post (Update comp)
            if useHistory then agent_History.Post (History_Add (round,comp))
        )







