module agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component

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

type agent_Components() =
    let mutable _map = Map.empty<ComponentID,Component>

    let idMan = new agent_IDManager()
    let agent_Current =
        
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

    member _.Add comp = agent_Current.Post (Add comp)
    member _.AddMany comps = agent_Current.Post (AddMany comps)
    member _.Get cid = //agent_Current.PostAndReply (fun replyChannel -> Get (cid,replyChannel))
        match _map.ContainsKey cid with
        | false -> None
        | true -> Some (_map.Item cid)
    member me.GetMany cids = //agent_Current.PostAndReply (fun replyChannel -> GetMany (cids,replyChannel))
        cids |> Array.Parallel.choose me.Get
    member _.GetMap = _map // agent_Current.PostAndReply GetMap
    member _.NewComponentID() = ComponentID(idMan.GetNewID())
    member _.Init startMap =
        _map <- startMap
        //agent_Current.Post (Init startMap)
        //idMan.Init (MapKeys startMap |> Seq.map (fun k -> k.ToUint32) |> Seq.max)
    member _.Remove c = agent_Current.Post (Remove c)
    member _.RemoveMany cts = agent_Current.Post (RemoveMany cts)
    member _.Update comp = agent_Current.Post (Update comp)







(*
let mutable _history = Map.empty<ComponentID,(RoundNumber*Component option)[]>

type private agent_HistoryMsg =
    | History_Add of RoundNumber * Component
    | History_AddMany of RoundNumber * Component[]
    | History_Init of Map<ComponentID,(RoundNumber*Component option)[]>
    | History_Remove of RoundNumber * Component
    | History_RemoveMany of RoundNumber * Component[]
 
let getHistory round cid = 
    match (_history.ContainsKey cid) with
    | false -> None
    | true -> searchArrayDataForRound round (_history.Item cid)

   
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
*)