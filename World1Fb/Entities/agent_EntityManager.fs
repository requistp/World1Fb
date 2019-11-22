module agent_EntityManager
open agent_Components
open agent_IDManager
open CommonGenericFunctions
open Component


//type private agent_HistoryMsg = 
//    | AddHistory of RoundNumber * EntityID * ComponentID[]
//    //| EntityExists of round:RoundNumber * entityID:uint32 * exists:AsyncReplyChannel<bool>
//    | GetHistory of EntityID * AsyncReplyChannel<(RoundNumber*ComponentID[] option)[]>
//    //| UpdateComponents of round:RoundNumber * entityID:EntityID * compIDs:int[]
//    //| GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
//    //| InitEntities of Map<uint32,Component[]>
//    //| ReplaceComponent of componentID:int * comp:Component

type private agent_CurrentMsg = 
    | Add of Component[]
    | AddMany of Component[][]
    | Get of EntityID * AsyncReplyChannel<ComponentID[]>
    | GetMany of EntityID[] * AsyncReplyChannel<ComponentID[][]>
    | Remove of EntityID
    //| GetEntityMap of AsyncReplyChannel<Map<uint32,Component[]> >
    //| InitEntities of Map<uint32,Component[]>

type agent_EntityManager(compMan:agent_Components) =

    let idMan = new agent_IDManager()

    //let agent_History =
    //    let mutable _map = Map.empty<EntityID,(RoundNumber*ComponentID[] option)[]>
    //    MailboxProcessor<agent_EntitiesMsg>.Start(
    //        fun inbox ->
    //            async { 
    //                while true do
    //                    let! msg = inbox.Receive()
    //                    match msg with
    //                    | AddHistory (round,eid,compIDs) ->
    //                        _map <- _map.Add(eid,[|(round,Some compIDs)|])
    //                    | GetHistory (eid,replyChannel) ->
    //                        match _map.ContainsKey eid with
    //                        | false -> replyChannel.Reply(Array.empty)
    //                        | true -> replyChannel.Reply(_map.Item eid)
    //                    //| Remove (round,eid) ->
    //                    //    match _map.ContainsKey(eid) with
    //                    //    | false -> ()
    //                    //    | true ->
    //                    //        let history =_map.Item(eid)
    //                    //        match (snd history.[0]) with
    //                    //        | None -> ()
    //                    //        | Some _ -> 
    //                    //            _map <- _map.Remove(eid).Add(eid,Array.append [|(round,None)|] history)
    //            }
    //        )
    
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
                        | Remove eid ->
                            if (_map.ContainsKey eid) then
                                _map <- _map.Remove(eid)
                }
            )

    member _.Add (round:RoundNumber) cts = 
        agent_Current.Post (Add cts)
        // Fix History
    member _.AddMany (round:RoundNumber) ctss = 
        agent_Current.Post (AddMany ctss)
        // Fix History
    member _.Get (round:RoundNumber option) eid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel)) // FIX
        |> compMan.GetMany round
    member _.GetMany (round:RoundNumber option) eids = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> GetMany (eids,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> GetMany (eids,replyChannel)) // FIX
        |> Array.map (compMan.GetMany round)
    member _.GetIDs (round:RoundNumber option) eid = 
        match round with
        | None -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel))
        | Some r -> agent_Current.PostAndReply (fun replyChannel -> Get (eid,replyChannel)) // FIX
    member _.NewEntityID() = EntityID(idMan.GetNewID())
    member _.Remove (round:RoundNumber) eid = 
        agent_Current.Post (Remove eid)
        // FIX history


