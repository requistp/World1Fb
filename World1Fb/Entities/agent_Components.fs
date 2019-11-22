module agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component


type private agent_CurrentMsg = 
    | Add of Component
    | AddMany of Component[]
    | Get of ComponentID * AsyncReplyChannel<Component option>
    | GetMany of ComponentID[] * AsyncReplyChannel<Component[]>
    | Init of Map<ComponentID,Component>
    | Remove of Component
    | RemoveMany of Component[]
    | Update of Component


type agent_Components() =

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
                                match _map.ContainsKey comp.ID with
                                | false -> _map.Add(comp.ID,comp)
                                | true -> _map.Remove(comp.ID).Add(comp.ID,comp)
                        let remove (comp:Component) =
                            if (_map.ContainsKey comp.ID) then
                                _map <- _map.Remove(comp.ID)
                        match msg with
                        | Add comp -> add comp
                        | AddMany cts -> cts |> Array.iter add
                        | Get (cid,replyChannel) ->
                            replyChannel.Reply(
                                match _map.ContainsKey cid with
                                | false -> None
                                | true -> Some (_map.Item(cid)))
                        | GetMany (cids,replyChannel) ->
                            replyChannel.Reply(
                                cids 
                                |> Array.choose (fun cid ->
                                    match _map.ContainsKey cid with
                                    | false -> None
                                    | true -> Some (_map.Item cid)))
                        | Init startMap ->
                            _map <- startMap
                        | Remove c -> remove c
                        | RemoveMany cts -> cts |> Array.iter remove
                        | Update comp ->
                            _map <-
                                match _map.ContainsKey(comp.ID) with
                                | false -> _map.Add(comp.ID,comp)
                                | true -> _map.Remove(comp.ID).Add(comp.ID,comp)
                }
            )

    member _.Add (round:RoundNumber) comp = 
        agent_Current.Post (Add comp)
        // Fix history
    member _.AddMany (round:RoundNumber) comps = 
        agent_Current.Post (AddMany comps)
        // Fix history
    member _.Get (round:RoundNumber option) cid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (cid,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (cid,replyChannel)) // FIX
    member _.GetMany (round:RoundNumber option) cids = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> GetMany (cids,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> GetMany (cids,replyChannel)) // FIX
    member _.NewComponentID() = ComponentID(idMan.GetNewID())
    member _.Init (startMap:Map<ComponentID,Component>) =
        agent_Current.Post (Init startMap)
        idMan.Init (MapKeys startMap |> Seq.map (fun k -> k.ToUint32) |> Seq.max)
    member _.Remove (round:RoundNumber) c = 
        agent_Current.Post (Remove c)
        //Fix history
    member _.RemoveMany (round:RoundNumber) cts = 
        agent_Current.Post (RemoveMany cts)
        //Fix history
    member _.Update (round:RoundNumber) comp = 
        agent_Current.Post (Update comp)
        //Fix History



    (*
    let agent_ComponentHistory =
         let mutable _map = Map.empty<ComponentID,(RoundNumber*Component option)[]>
         MailboxProcessor<agent_ComponentsMsg>.Start(
             fun inbox ->
                 async { 
                     while true do
                         let! msg = inbox.Receive()
                         match msg with
                         | Add (round,comp) ->
                             _map <- _map.Add(comp.ID,[|(round,Some comp)|])
                         | Get (cid,replyChannel) ->
                             replyChannel.Reply(
                                 match _map.ContainsKey(cid) with
                                 | false -> [||]
                                 | true -> _map.Item(cid))
                         | Init startMap ->
                             _map <- startMap
                         | Remove (round,cid) ->
                             match _map.ContainsKey(cid) with
                             | false -> ()
                             | true ->
                                 let a = _map.Item(cid)
                                 match (snd a.[0]).IsNone with
                                 | true -> () 
                                 | false -> 
                                     _map <- _map.Remove(cid).Add(cid,Array.append [|(round,None)|] a)
                         | Update (round,comp) ->
                             match _map.ContainsKey(comp.ID) with
                             | false -> _map <- _map.Add(comp.ID,[|(round,Some comp)|])
                             | true ->
                                 let a = _map.Item(comp.ID)
                                 _map <- _map.Remove(comp.ID).Add(comp.ID,Array.append [|(round,Some comp)|] a)
                 }
             )

     member _.Add round comp = agent_ComponentHistory.Post (Add (round,comp))
     member _.Get round cid = agent_ComponentHistory.PostAndReply (fun replyChannel -> Get (cid,replyChannel)) |> searchArrayDataForRound round
     member _.NewComponentID() = idMan.GetNewID()
     member _.Init (startMap:Map<ComponentID,(RoundNumber*Component option)[]>) =
         agent_ComponentHistory.Post (Init startMap)
         idMan.Init (MapKeys startMap |>Seq.max)
     member _.Remove round (comp:Component) = agent_ComponentHistory.Post (Remove (round,comp.ID))
     member _.Update round comp = agent_ComponentHistory.Post (Update (round,comp))


*)