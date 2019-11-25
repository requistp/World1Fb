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
        
type agent_ComponentTypes(compMan:agent_Components) = 
    let mutable _map = Map.empty<ComponentType,ComponentID[]>
    let agent_Current =
        
        MailboxProcessor<agent_CurrentMsg>.Start(
            fun inbox ->
                async { 
                    while true do
                        let! msg = inbox.Receive()
                        let add (comp:Component) = 
                            let ctid = GetComponentType comp
                            let cid = GetComponentID comp
                            _map <- 
                                match (_map.ContainsKey ctid) with
                                | false -> _map.Add(ctid,[|cid|])
                                | true -> 
                                    let newArray = _map.Item(ctid) |> Array.filter (fun c -> c <> cid) |> Array.append [|cid|]
                                    _map.Remove(ctid).Add(ctid,newArray) 
                        let remove (comp:Component) =
                            let ctid = GetComponentType comp
                            if (_map.ContainsKey(ctid)) then
                                let others = _map.Item(ctid) |> Array.filter (fun c -> c <> GetComponentID comp)
                                _map <- _map.Remove(ctid).Add(ctid,others) 
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
    
    member _.Add comp = agent_Current.Post (Add comp)
    member _.AddMany cts = agent_Current.Post (AddMany cts)
    member _.Get ctid = //agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) |> compMan.GetMany
        match _map.ContainsKey ctid with
        | false -> Array.empty
        | true -> _map.Item ctid
        |> compMan.GetMany
    member _.GetMap = _map //agent_Current.PostAndReply GetMap     //old, i think |> Map.map (fun _ cids -> cids |> Array.choose compMan.Get)
    member _.Init startMap = _map <- startMap //agent_Current.Post (Init startMap)
    member _.Remove (comp:Component) = agent_Current.Post (Remove comp)
    member _.RemoveMany (cts:Component[]) = agent_Current.Post (RemoveMany cts)




    //member _.GetIDs round ctid = 
    //    match round with
    //    | None -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel))
    //    | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (ctid,replyChannel)) // FIX    //member _.Add round ctid cid = agent_History.Post (AddHistory (round,ctid,cid))
    //member _.Get round ctid = agent_History.PostAndReply (fun replyChannel -> GetHistory (ctid,replyChannel)) |> searchArrayDataForRound round
    //member _.Remove round (comp:Component) = agent_History.Post (RemoveHistory (round,comp.ComponentTypeID,comp.ID))

(*
type private agent_HistoryMsg = 
    | History_Add of RoundNumber * Component
    | History_AddMany of RoundNumber * Component[]
    | History_Init of Map<ComponentType,(RoundNumber*ComponentID[] option)[]>
    | History_Remove of RoundNumber * Component
    | History_RemoveMany of RoundNumber * Component[]
        
let mutable _history = Map.empty<ComponentType,(RoundNumber*ComponentID[] option)[]>

let getHistory round ctid = 
    match (_history.ContainsKey ctid) with
    | false -> [||]
    | true -> 
        match searchArrayDataForRound round (_history.Item ctid) with
        | None -> [||]
        | Some a -> a

let agent_History =
    MailboxProcessor<agent_HistoryMsg>.Start(
        fun inbox ->
            async { 
                while true do
                    let! msg = inbox.Receive()
                    let add round (c:Component) = 
                        let ctid = GetComponentType c
                        let cid = GetComponentID c
                        _history <- 
                            match (_history.ContainsKey ctid) with
                            | false -> _history.Add(ctid,[|round,Some [|cid|]|])
                            | true -> 
                                let cids = 
                                    match (snd (_history.Item(ctid).[0])) with
                                    | None -> [|cid|]
                                    | Some cids -> cids |> Array.filter (fun id -> id <> cid) |> Array.append [|cid|]
                                let newArray = Array.append [|round,Some cids|] (_history.Item(ctid))
                                _history.Remove(ctid).Add(ctid,newArray)
                    let remove round (c:Component) =
                        let ctid = GetComponentType c
                        _history <- 
                            match (_history.ContainsKey ctid) with
                            | false -> _history.Add(ctid,[|round,None|])
                            | true -> 
                                let cidso = 
                                    match (snd (_history.Item(ctid).[0])) with
                                    | None -> None
                                    | Some cids -> Some (cids |> Array.filter (fun id -> id <> GetComponentID c))
                                let newArray = Array.append [|round,cidso|] (_history.Item ctid)
                                _history.Remove(ctid).Add(ctid,newArray)
                    match msg with
                    | History_Add (round,c) -> add round c
                    | History_AddMany (round,cs) -> cs |> Array.iter (add round)
                    | History_Init startMap -> _history <- startMap
                    | History_Remove (round,c) -> remove round c
                    | History_RemoveMany (round,cs) -> cs |> Array.iter (remove round)
            }
        )
*)